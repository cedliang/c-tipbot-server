{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           DbConnections
import           TipBackend
import           Control.Exception (bracket)
import           Types.Errors
import qualified Data.ByteString as B
import           Types.SchemaTypes

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

type TipbotApi = "backend" :> Capture "backendID" Int
  :> ("user" :> Capture "userdid" Int
      :> (Get '[JSON] CValue :<|> ReqBody '[JSON] CValue
          :> Post '[JSON] CValue :<|> "transfer" :> Capture "destDid" Int
          :> ReqBody '[JSON] CValue :> Post '[JSON] [CValue]) :<|> "tokens"
      :> (Get '[JSON] [Token]
          :<|> "add" :> ReqBody '[JSON] Token :> Post '[JSON] [Token]))

server1 :: ManagersMap -> Server TipbotApi
server1 = backendServer
  where
    backendServer manMap backendId =
      userServer manMap backendId :<|> tokenServer manMap backendId

    userServer manMap backendId did = epUserBalance manMap backendId did
      :<|> epModifyUserBalance manMap backendId did
      :<|> epTransferUserBalance manMap backendId did

    tokenServer manMap backendId =
      epListTokens manMap backendId :<|> epAddToken manMap backendId

epAddToken :: ManagersMap -> Int -> Token -> Handler [Token]
epAddToken manMap backendId addTok = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  addTokOp
    <- bracketEndpointAction connsChan $ addBackendToken writeLock addTok
  throwOnLeft (throwError . addTokenError) addTokOp
    $ const
    $ epListTokens manMap backendId

epListTokens :: ManagersMap -> Int -> Handler [Token]
epListTokens manMap backendId = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  toks <- bracketEndpointAction connsChan listBackendTokens
  throwOnLeft (throwError . listTokensError) toks pure

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

listTokensError :: OperationError -> ServerError
listTokensError e = let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
                    in err404 { errBody = "Could not get token list: " <> eBS }

addTokenError :: OperationError -> ServerError
addTokenError e =
  let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
  in err404 { errBody = "Could not add token to backend: " <> eBS }

