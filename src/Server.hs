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
import           TipBackend (CValue, getUserBalance)
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

type TipbotApi = "backend" :> Capture "backendID" Int :> "user"
  :> Capture "userdid" Int :> (Get '[JSON] CValue :<|> Post '[JSON] CValue)

server1 :: ManagersMap -> Server TipbotApi
server1 = userServer
  where
    userServer manMap backendId did =
      userBalance manMap backendId did :<|> userBalance' manMap backendId did

userBalance :: ManagersMap -> Int -> Int -> Handler CValue
userBalance manMap backendId did = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- liftIO
    $ bracket (readChan connsChan) (writeChan connsChan) (getUserBalance did)
  throwOnLeft (throwError . txError) rUserBalance pure

userBalance' :: ManagersMap -> Int -> Int -> Handler CValue
userBalance' manMap backendId did = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- liftIO
    $ bracket (readChan connsChan) (writeChan connsChan) (getUserBalance did)
  throwOnLeft (throwError . txError) rUserBalance pure

txError :: OperationError -> ServerError
txError e = let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
            in err404 { errBody = "UserID does not exist: " <> eBS }

