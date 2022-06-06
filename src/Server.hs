{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Control.Exception (bracket)
import           Control.Monad
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           DbConnections
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           TipBackend
import           Types.Errors
import           Types.SchemaTypes
import           Types.SchemaOps

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
      :> (Get '[JSON] [Token] :<|> "add"
          :> ReqBody '[JSON] Token :> Post '[JSON] [Token] :<|> "aliases"
          :> Capture "assetid" Text :> Get '[JSON] [TokenAlias]) :<|> "alias"
      :> (Capture "tokenalias" Text
          :> (Get '[JSON] TokenAlias
              :<|> Capture "tokenname" Text :> Post '[JSON] [TokenAlias])))

server1 :: ManagersMap -> Server TipbotApi
server1 = backendServer
  where
    backendServer manMap backendId = userServer manMap backendId
      :<|> tokenServer manMap backendId
      :<|> aliasServer manMap backendId

    userServer manMap backendId did = epUserBalance manMap backendId did
      :<|> epModifyUserBalance manMap backendId did
      :<|> epTransferUserBalance manMap backendId did

    tokenServer manMap backendId = epListTokens manMap backendId
      :<|> epAddToken manMap backendId
      :<|> epGetTokenAliases manMap backendId

    aliasServer manMap backendId al =
      epGetAlias manMap backendId al :<|> epAddAlias manMap backendId al

epGetTokenAliases :: ManagersMap -> Int -> Text -> Handler [TokenAlias]
epGetTokenAliases manMap backendId asid = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  tokAliases <- bracketEndpointAction connsChan $ getTokenAliases asid
  throwOnLeft
    (throwError . mk404ServerError "Could not get token aliases: ")
    tokAliases
    pure

epGetAlias :: ManagersMap -> Int -> Text -> Handler TokenAlias
epGetAlias manMap backendId al = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  tokAlias <- bracketEndpointAction connsChan $ getAlias al
  throwOnLeft
    (throwError . mk404ServerError "Alias does not exist: ")
    tokAlias
    pure

epAddAlias :: ManagersMap -> Int -> Text -> Text -> Handler [TokenAlias]
epAddAlias manMap backendId al asid = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  addAliasTx <- bracketEndpointAction connsChan $ addAlias writeLock al asid
  throwOnLeft
    (throwError . mk404ServerError "Could not add alias: ")
    addAliasTx
    $ const
    $ epGetTokenAliases manMap backendId asid

epAddToken :: ManagersMap -> Int -> Token -> Handler [Token]
epAddToken manMap backendId addTok = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  addTokOp
    <- bracketEndpointAction connsChan $ addBackendToken writeLock addTok
  throwOnLeft
    (throwError . mk404ServerError "Could not add token to backend: ")
    addTokOp
    $ const
    $ epListTokens manMap backendId

epListTokens :: ManagersMap -> Int -> Handler [Token]
epListTokens manMap backendId = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  toks <- bracketEndpointAction connsChan listBackendTokens
  throwOnLeft
    (throwError . mk404ServerError "Could not get token list: ")
    toks
    pure

epUserBalance :: ManagersMap -> Int -> Int -> Handler CValue
epUserBalance manMap backendId did = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan $ getUserBalance did
  throwOnLeft
    (throwError . mk404ServerError "UserID does not exist: ")
    rUserBalance
    pure

epModifyUserBalance :: ManagersMap -> Int -> Int -> CValue -> Handler CValue
epModifyUserBalance manMap backendId did diffValue = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan
    $ modifyUserBalance writeLock did diffValue
  throwOnLeft
    (throwError . mk404ServerError "Token transaction failure: ")
    rUserBalance
    $ const
    $ epUserBalance manMap backendId did

epTransferUserBalance
  :: ManagersMap -> Int -> Int -> Int -> CValue -> Handler [CValue]
epTransferUserBalance manMap backendId sourceDid destDid diffValue = do
  (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
  rUserBalance <- bracketEndpointAction connsChan
    $ transferUserBalance writeLock (sourceDid, diffValue) destDid
  throwOnLeft
    (throwError . mk404ServerError "Token transaction failure: ")
    rUserBalance
    $ const
    $ zipWithM
      ($)
      (replicate 2 $ epUserBalance manMap backendId)
      [sourceDid, destDid]

mk404ServerError :: ByteString -> OperationError -> ServerError
mk404ServerError description e =
  let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
  in err404 { errBody = description <> eBS }

