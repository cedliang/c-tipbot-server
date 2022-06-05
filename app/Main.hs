{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import qualified Data.Map as Map
import           Database.SQLite.Simple
import           Server (servantIO)
import           TipBackend

main = servantIO
-- main :: IO ()
-- main = do
--   connsChan <- openConnChan "tipbot-records.sqlite" 10
--   writeLock <- newMVar ()
--   putStrLn "STARTING BALANCES: "
--   putStrLn "123:"
--     >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 123)
--     >>= print
--   putStrLn "456:"
--     >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
--     >>= print
--   startQsem <- newQSemN 0
--   -- add user test
--   -- forkIO
--   --   $ bracket
--   --     (readChan connsChan)
--   --     (writeChan connsChan)
--   --     (addNewUser writeLock 456)
--   --   >>= putStrLn
--   --   >> signalQSemN startQsem 1
--   -- get balance test
--   replicateM_ 100
--     $ forkIO
--     $ bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
--     >>= (\r -> do
--            either print (print . encode) r)
--     >> signalQSemN startQsem 1
--   -- credit balance test
--   -- forkIO
--   --   $ bracket
--   --     (readChan connsChan)
--   --     (writeChan connsChan)
--   --     (modifyUserBalance writeLock 123
--   --      $ Value 10000000
--   --      $ Map.fromList [("test2", 100)])
--   --   >>= print
--   --   >> signalQSemN startQsem 1
--   -- transfer balance test
--   -- replicateM 100
--   --   $ forkIO
--   --   $ bracket
--   --     (readChan connsChan)
--   --     (writeChan connsChan)
--   --     (transferUserBalance
--   --        writeLock
--   --        (123, Value 1 $ Map.fromList [("test2", 0), ("testtoken", (0))])
--   --        456)
--   --   -- >>= print
--   --   >> signalQSemN startQsem 1
--   waitQSemN startQsem 1
--   putStrLn "\n\nENDING BALANCES: "
--   putStrLn "123:"
--     >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 123)
--     >>= print
--   putStrLn "456:"
--     >> bracket (readChan connsChan) (writeChan connsChan) (getUserBalance 456)
--     >>= print
-- openConnChan :: String -> Int -> IO (Chan Connection)
-- openConnChan connName numConns = do
--   conns <- replicateM numConns $ open connName
--   traverse_ initialiseConn conns
--   connsChan <- newChan
--   writeList2Chan connsChan conns
--   pure connsChan
-- initialiseConn :: Connection -> IO ()
-- initialiseConn conn = do
--   execute_ conn "PRAGMA foreign_keys = ON"
