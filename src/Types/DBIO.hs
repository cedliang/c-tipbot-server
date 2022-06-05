module Types.DBIO where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Database.SQLite.Simple
import           Types.Errors

writeTransact :: MVar ()
              -> Connection
              -> IO ()
              -> ExceptT OperationError IO (Either SQLError ())
writeTransact writeLock conn ioacts = lift
  $ try
  $ bracket (takeMVar writeLock) (putMVar writeLock)
  $ const
  $ withImmediateTransaction conn ioacts

