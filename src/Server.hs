{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite.Simple
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
epGetTokenAliases manMap backendId asid =
    epAction
        manMap
        backendId
        "Could not get token aliases: "
        (const $ getTokenAliases asid)
        pure

epGetAlias :: ManagersMap -> Int -> Text -> Handler TokenAlias
epGetAlias manMap backendId al =
    epAction
        manMap
        backendId
        "Alias does not exist: "
        (const $ getAlias al)
        pure

epAddAlias :: ManagersMap -> Int -> Text -> Text -> Handler [TokenAlias]
epAddAlias manMap backendId al asid =
    epAction
        manMap
        backendId
        "Could not add alias: "
        (addAlias al asid)
        (const $ epGetTokenAliases manMap backendId asid)

epAddToken :: ManagersMap -> Int -> Token -> Handler [Token]
epAddToken manMap backendId addTok =
    epAction
        manMap
        backendId
        "Could not add token to backend: "
        (addBackendToken addTok)
        (const $ epListTokens manMap backendId)

epListTokens :: ManagersMap -> Int -> Handler [Token]
epListTokens manMap backendId =
    epAction
        manMap
        backendId
        "Could not get token list: "
        (const listBackendTokens)
        pure

epUserBalance :: ManagersMap -> Int -> Int -> Handler CValue
epUserBalance manMap backendId did =
    epAction
        manMap
        backendId
        "UserID does not exist: "
        (const $ getUserBalance did)
        pure

epModifyUserBalance :: ManagersMap -> Int -> Int -> CValue -> Handler CValue
epModifyUserBalance manMap backendId did diffValue =
    epAction
        manMap
        backendId
        "Token modifybalance failure: "
        (modifyUserBalance did diffValue)
        (const $ epUserBalance manMap backendId did)

epTransferUserBalance ::
    ManagersMap -> Int -> Int -> Int -> CValue -> Handler [CValue]
epTransferUserBalance manMap backendId sourceDid destDid diffValue =
    epAction
        manMap
        backendId
        "Token transfer failure: "
        (transferUserBalance (sourceDid, diffValue) destDid)
        ( const $
            zipWithM
                ($)
                (replicate 2 $ epUserBalance manMap backendId)
                [sourceDid, destDid]
        )

epAction ::
    ManagersMap ->
    Int ->
    ByteString ->
    ( MVar () ->
      Connection ->
      IO (Either OperationError b1)
    ) ->
    (b1 -> Handler b2) ->
    Handler b2
epAction manMap backendId errStr retrieveAct successAct = do
    (connsChan, writeLock) <- handlerManMap existentManagerIds manMap backendId
    either
        (throwError . mk404ServerError errStr)
        successAct
        =<< bracketEndpointAction connsChan (retrieveAct writeLock)
