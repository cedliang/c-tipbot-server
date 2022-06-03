{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TipbotServer where

import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Database.SQLite.Simple (Connection)
import           Control.Concurrent

data ManagerResources =
  ManagerResources { connsChan :: Chan Connection, writeLock :: MVar () }

data Config = Config { hostPort :: Int }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { hostPort = 7069 }

serveSettings :: Settings
serveSettings = (setPort (hostPort defaultConfig) . setHost "127.0.0.1")
  defaultSettings

servantIO :: IO ()
servantIO = do
  runSettings serveSettings $ (simpleCors . logStdoutDev) $ servApp

servApp :: Application
servApp = serve (Proxy :: Proxy TipbotApi) server1

type TipbotApi =
  "adahandle" :> Capture "inputHandle" TL.Text :> Get '[PlainText] TL.Text

server1 :: ServerT TipbotApi Handler
server1 = baseHandler

baseHandler :: TL.Text -> Handler TL.Text
baseHandler _ = pure "123"
