{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Database.SQLite.Simple (Connection, execute_, open)
import           Control.Concurrent
import           DbConnections
import           TipBackend (CValue, getUserBalance, modifyUserBalance
                           , transferUserBalance)
import           Data.Aeson
import           Data.Maybe (fromMaybe)
import           Control.Exception (bracket)
import           Types.Errors
import           Control.Monad.Except
import qualified Data.ByteString as B
import           TextShow

servantIO :: IO ()
servantIO = do
  mapM_ initialiseDbs existentManagerIds
  manMap <- mkManagersMap existentManagerIds
  runSettings serveSettings $ (simpleCors . logStdoutDev) $ servApp manMap

existentManagerIds :: [Int]
existentManagerIds = [1, 2, 3]

data Config = Config { hostPort :: Int, hostAddr :: HostPreference }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { hostPort = 7069, hostAddr = "127.0.0.1" }

serveSettings :: Settings
serveSettings =
  (setPort (hostPort defaultConfig) . setHost (hostAddr defaultConfig))
    defaultSettings

servApp :: ManagersMap -> Application
servApp = serve (Proxy :: Proxy TipbotApi) . server1

type TipbotApi = "backend"
  :> Capture "backendID" Int :> "user" :> Capture "userdid" Int
  :> (Get '[JSON] CValue :<|> ReqBody '[JSON] CValue
      :> Post '[JSON] CValue :<|> "transfer" :> Capture "destDid" Int
      :> ReqBody '[JSON] CValue :> Post '[JSON] [CValue])

server1 :: ManagersMap -> Server TipbotApi
server1 = userServer
  where
    userServer manMap backendId did = epUserBalance manMap backendId did
      :<|> epModifyUserBalance manMap backendId did
      :<|> epTransferUserBalance manMap backendId did

epUserBalance :: ManagersMap -> Int -> Int -> Handler CValue
epUserBalance manMap backendId did = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan $ getUserBalance did
  throwOnLeft (throwError . userExistsError) rUserBalance pure

epModifyUserBalance :: ManagersMap -> Int -> Int -> CValue -> Handler CValue
epModifyUserBalance manMap backendId did diffValue = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan
    $ modifyUserBalance writeLock did diffValue
  throwOnLeft (throwError . tokenTransactionError) rUserBalance
    $ const
    $ epUserBalance manMap backendId did

epTransferUserBalance
  :: ManagersMap -> Int -> Int -> Int -> CValue -> Handler [CValue]
epTransferUserBalance manMap backendId sourceDid destDid diffValue = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan
    $ transferUserBalance writeLock (sourceDid, diffValue) destDid
  throwOnLeft (throwError . tokenTransactionError) rUserBalance
    $ const
    $ zipWithM
      ($)
      (replicate 2 $ epUserBalance manMap backendId)
      [sourceDid, destDid]

tokenTransactionError :: OperationError -> ServerError
tokenTransactionError e =
  let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
  in err404 { errBody = "Token transaction failure: " <> eBS }

userExistsError :: OperationError -> ServerError
userExistsError e = let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
                    in err404 { errBody = "UserID does not exist: " <> eBS }
