{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DbManager
import           Types.SchemaTypes
import           Types.SchemaOps
import           Control.Concurrent
import           Database.SQLite.Simple
import           Control.Monad
import           Data.Foldable
import           Control.Concurrent.MVar
import           Control.Concurrent.QSemN
import           Control.Exception
import qualified Data.Map as Map
import           Data.UnixTime

main :: IO ()
main = do
  connsChan <- openConnChan "tipbot-records.sqlite" 10
  writeLock <- newMVar ()
  putStrLn "STARTING BALANCES: "
  putStrLn "123:"
    >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 123)
    >>= print
  putStrLn "456:"
    >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
    >>= print
  stime <- getUnixTime
  startQsem <- newQSemN 0
  -- add user test
  -- forkIO
  --   $ bracket
  --     (readChan connsChan)
  --     (writeChan connsChan)
  --     (addNewUser writeLock 456)
  --   >>= putStrLn
  --   >> signalQSemN startQsem 1
  -- get balance test
  replicateM 10000
    $ forkIO
    $ bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
    -- >>= print
    >> signalQSemN startQsem 1
  -- credit balance test
  -- forkIO
  --   $ bracket
  --     (readChan connsChan)
  --     (writeChan connsChan)
  --     (modifyUserBalance writeLock 123
  --      $ Value 10000000
  --      $ Map.fromList [("test2", 100)])
  --   >>= print
  --   >> signalQSemN startQsem 1
  -- transfer balance test
  replicateM 10000
    $ forkIO
    $ bracket
      (readChan connsChan)
      (writeChan connsChan)
      (transferUserBalance
         writeLock
         (123, Value 1 $ Map.fromList [("test2", 0), ("testtoken", (0))])
         456)
    -- >>= print
    >> signalQSemN startQsem 1
  waitQSemN startQsem 20000
  getUnixTime >>= print . flip diffUnixTime stime
  putStrLn "\n\nENDING BALANCES: "
  putStrLn "123:"
    >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 123)
    >>= print
  putStrLn "456:"
    >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
    >>= print

openConnChan :: String -> Int -> IO (Chan Connection)
openConnChan connName numConns = do
  conns <- replicateM numConns $ open connName
  traverse_ initialiseConn conns
  connsChan <- newChan
  writeList2Chan connsChan conns
  pure connsChan

initialiseConn :: Connection -> IO ()
initialiseConn conn = do
  execute_ conn "PRAGMA foreign_keys = ON"
-- query_ conn "SELECT (id) FROM tokens WHERE id=-1"