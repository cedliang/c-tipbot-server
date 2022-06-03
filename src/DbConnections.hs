{-# LANGUAGE OverloadedStrings #-}

module DbConnections where

import           Control.Monad
import           Database.SQLite.Simple (Connection, execute_, open)
import           Control.Concurrent
import Servant

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

data ManagerResources =
  ManagerResources { connsChan :: Chan Connection, writeLock :: MVar () }

type ManagersMap = [(Int, ManagerResources)]

mkManagerResources :: Int -> Int -> IO ManagerResources
mkManagerResources numConns managerid = ManagerResources
  <$> openConnChan (show managerid <> "_tipbot-records.sqlite3") numConns
  <*> newMVar ()

mkManagersMap :: [Int] -> IO ManagersMap
mkManagersMap lids = forM lids
  $ \lid -> do
    (lid,) <$> mkManagerResources 10 lid

getManResources :: ManagersMap -> Int -> Maybe (Chan Connection, MVar ())
getManResources manMap lid = case lookup lid manMap of
  Just manRec -> Just (connsChan manRec, writeLock manRec)
  _ -> Nothing

-- reusable component for retrieving connection and write lock
handlerManMap :: [Int] -> ManagersMap -> Int -> Handler (Chan Connection, MVar ())
handlerManMap exManIds manMap backendId = do
  when (backendId `notElem` exManIds) $ throwError backendDoesntExist
  maybe (throwError connectionFailure) pure $ getManResources manMap backendId
  where
    connectionFailure :: ServerError
    connectionFailure =
      err500 { errBody = "Couldn't get DB connection resources." }

    backendDoesntExist :: ServerError
    backendDoesntExist =
      err404 { errBody = "Tipbot backend ID does not exist." }
