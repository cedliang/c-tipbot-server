{-# LANGUAGE OverloadedStrings #-}

module DbConnections where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Except
import Data.ByteString qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite.Simple
  ( Connection,
    close,
    execute_,
    open,
    withTransaction,
  )
import Servant
import Types.Errors

initialiseConn :: Connection -> IO ()
initialiseConn conn = do
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"

openConnChan :: String -> Int -> IO (Chan Connection)
openConnChan connName numConns = do
  conns <- replicateM numConns $ open connName
  mapM_ initialiseConn conns
  connsChan <- newChan
  writeList2Chan connsChan conns
  pure connsChan

bracketEndpointAction ::
  Chan Connection ->
  (Connection -> IO (Either OperationError a)) ->
  Servant.Handler (Either OperationError a)
bracketEndpointAction connsChan = liftIO . bracket (readChan connsChan) (writeChan connsChan)

mk404ServerError :: ByteString -> OperationError -> ServerError
mk404ServerError description e =
  let eBS = B.fromStrict $ T.encodeUtf8 $ T.pack $ show e
   in err404 {errBody = description <> eBS}

data ManagerResources = ManagerResources {connsChan :: Chan Connection, writeLock :: MVar ()}

type ManagersMap = [(Int, ManagerResources)]

mkManagerResources :: Int -> Int -> IO ManagerResources
mkManagerResources numConns managerid =
  ManagerResources
    <$> openConnChan (show managerid <> "_tipbot-records.sqlite3") numConns
    <*> newMVar ()

mkManagersMap :: [Int] -> IO ManagersMap
mkManagersMap lids = forM lids $
  \lid -> (lid,) <$> mkManagerResources 5 lid

getManResources :: ManagersMap -> Int -> Maybe (Chan Connection, MVar ())
getManResources manMap lid = case lookup lid manMap of
  Just manRec -> Just (connsChan manRec, writeLock manRec)
  _ -> Nothing

-- reusable component for retrieving connection and write lock
handlerManMap ::
  [Int] -> ManagersMap -> Int -> Servant.Handler (Chan Connection, MVar ())
handlerManMap exManIds manMap backendId = do
  when (backendId `notElem` exManIds) $ throwError backendDoesntExist
  maybe (throwError connectionFailure) pure $ getManResources manMap backendId
  where
    connectionFailure :: ServerError
    connectionFailure =
      err500 {errBody = "Couldn't get DB connection resources."}

    backendDoesntExist :: ServerError
    backendDoesntExist =
      err404 {errBody = "Tipbot backend ID does not exist."}

initialiseDbs :: Int -> IO ()
initialiseDbs mid = do
  conn <- open (show mid <> "_tipbot-records.sqlite3")
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"
  withTransaction conn $
    do
      execute_
        conn
        " CREATE TABLE IF NOT EXISTS tokens \
        \ (id TEXT PRIMARY KEY, name TEXT NOT NULL, \
        \ decimals integer NOT NULL CHECK(decimals >= 0) default 0 \
        \ )"
      execute_
        conn
        " CREATE TABLE IF NOT EXISTS aliases ( \
        \ alias TEXT PRIMARY KEY, \
        \ assetid TEXT NOT NULL, \
        \ FOREIGN KEY (assetid) REFERENCES tokens(id) \
        \ )"
      execute_
        conn
        " CREATE TABLE IF NOT EXISTS user ( \
        \ did INTEGER PRIMARY KEY CHECK(did >= 0), \
        \ lovelace_balance INTEGER NOT NULL CHECK(lovelace_balance >= 0) DEFAULT 0, \
        \ c_addr TEXT NOT NULL \
        \ )"
      execute_
        conn
        " CREATE TABLE IF NOT EXISTS user_balance ( \
        \ token_id TEXT NOT NULL, \
        \ user_did INT NOT NULL, \
        \ amount INT NOT NULL, \
        \ FOREIGN KEY (token_id) REFERENCES tokens(id), \
        \ FOREIGN KEY (user_did) REFERENCES user(did), \
        \ CHECK(amount >= 0), \
        \ UNIQUE(token_id, user_did) \
        \ )"
  close conn
