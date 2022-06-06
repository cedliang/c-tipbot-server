{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Exception (bracket)
import Control.Monad
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import DbConnections
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.OpenApi
import TipBackend
import Types.Errors
import Types.SchemaTypes

servantIO :: IO ()
servantIO = do
    mapM_ initialiseDbs existentManagerIds
    manMap <- mkManagersMap existentManagerIds
    runSettings serveSettings $ (simpleCors . logStdoutDev) $ servApp manMap

existentManagerIds :: [Int]
existentManagerIds = [1, 2, 3]

data Config = Config {hostPort :: Int, hostAddr :: HostPreference}
    deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config{hostPort = 7069, hostAddr = "127.0.0.1"}

serveSettings :: Settings
serveSettings =
    (setPort (hostPort defaultConfig) . setHost (hostAddr defaultConfig))
        defaultSettings

servApp :: ManagersMap -> Application
servApp manMap = serve combinedProxy $ combinedServer manMap

combinedProxy :: Proxy CombinedApi
combinedProxy = Proxy

type CombinedApi = ApiDocApi :<|> TipbotApi

type ApiDocApi = "swagger.json" :> Get '[JSON] OpenApi

combinedServer :: ManagersMap -> Server CombinedApi
combinedServer manMap = apiServer manMap :<|> tipbotServer manMap
  where
    apiServer _ = pure $ toOpenApi (Proxy :: Proxy TipbotApi)

type TipbotApi =
    "backend" :> Capture "backendID" Int
        :> ( "user" :> Capture "userdid" Int
                :> ( Get '[JSON] CValue :<|> ReqBody '[JSON] CValue
                        :> Post '[JSON] CValue :<|> "transfer"
                        :> Capture "destDid" Int
                        :> ReqBody '[JSON] CValue
                        :> Post '[JSON] [CValue]
                   )
                    :<|> "tokens"
                :> ( Get '[JSON] [Token] :<|> "add"
                        :> ReqBody '[JSON] Token
                        :> Post '[JSON] [Token] :<|> "aliases"
                        :> Capture "assetid" Text
                        :> Get '[JSON] [TokenAlias]
                   )
                    :<|> "alias"
                :> ( Capture "tokenalias" Text
                        :> ( Get '[JSON] TokenAlias
                                :<|> Capture "tokenname" Text :> Post '[JSON] [TokenAlias]
                           )
                   )
           )

tipbotServer :: ManagersMap -> Server TipbotApi
tipbotServer manMap = backendServer manMap
  where
    backendServer manMap backendId =
        userServer manMap backendId
            :<|> tokenServer manMap backendId
            :<|> aliasServer manMap backendId

    userServer manMap backendId did =
        epUserBalance manMap backendId did
            :<|> epModifyUserBalance manMap backendId did
            :<|> epTransferUserBalance manMap backendId did

    tokenServer manMap backendId =
        epListTokens manMap backendId
            :<|> epAddToken manMap backendId
            :<|> epGetTokenAliases manMap backendId

    aliasServer manMap backendId al =
        epGetAlias manMap backendId al :<|> epAddAlias manMap backendId al

epGetTokenAliases :: ManagersMap -> Int -> Text -> Handler [TokenAlias]
epGetTokenAliases manMap backendId asid = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either (throwError . mk404ServerError "Could not get token aliases: ") pure
        =<< bracketEndpointAction connsChan (getTokenAliases asid)

epGetAlias :: ManagersMap -> Int -> Text -> Handler TokenAlias
epGetAlias manMap backendId al = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either (throwError . mk404ServerError "Alias does not exist: ") pure
        =<< bracketEndpointAction connsChan (getAlias al)

epAddAlias :: ManagersMap -> Int -> Text -> Text -> Handler [TokenAlias]
epAddAlias manMap backendId al asid = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either
        (throwError . mk404ServerError "Could not add alias: ")
        (const $ epGetTokenAliases manMap backendId asid)
        =<< bracketEndpointAction connsChan (addAlias writeLock al asid)

epAddToken :: ManagersMap -> Int -> Token -> Handler [Token]
epAddToken manMap backendId addTok = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either
        (throwError . mk404ServerError "Could not add token to backend: ")
        (const $ epListTokens manMap backendId)
        =<< bracketEndpointAction connsChan (addBackendToken writeLock addTok)

epListTokens :: ManagersMap -> Int -> Handler [Token]
epListTokens manMap backendId = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either (throwError . mk404ServerError "Could not get token list: ") pure
        =<< bracketEndpointAction connsChan listBackendTokens

epUserBalance :: ManagersMap -> Int -> Int -> Handler CValue
epUserBalance manMap backendId did = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either (throwError . mk404ServerError "UserID does not exist: ") pure
        =<< bracketEndpointAction connsChan (getUserBalance did)

epModifyUserBalance :: ManagersMap -> Int -> Int -> CValue -> Handler CValue
epModifyUserBalance manMap backendId did diffValue = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either
        (throwError . mk404ServerError "Token transaction failure: ")
        (const $ epUserBalance manMap backendId did)
        =<< bracketEndpointAction
            connsChan
            ( modifyUserBalance writeLock did diffValue
            )

epTransferUserBalance ::
    ManagersMap -> Int -> Int -> Int -> CValue -> Handler [CValue]
epTransferUserBalance manMap backendId sourceDid destDid diffValue = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either
        (throwError . mk404ServerError "Token transaction failure: ")
        ( const $
            zipWithM
                ($)
                (replicate 2 $ epUserBalance manMap backendId)
                [sourceDid, destDid]
        )
        =<< bracketEndpointAction
            connsChan
            (transferUserBalance writeLock (sourceDid, diffValue) destDid)

mk404ServerError :: ByteString -> OperationError -> ServerError
mk404ServerError description e =
    let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
     in err404{errBody = description <> eBS}
