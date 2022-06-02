module Types.DBIO where

import           Control.Monad.Except
import           Control.Concurrent
import           Errors
import           Database.SQLite.Simple
import           Control.Exception

writeTransact :: MVar ()
              -> Connection
              -> IO ()
              -> ExceptT OperationError IO (Either SQLError ())
writeTransact writeLock conn ioacts = lift
  $ try
  $ bracket (takeMVar writeLock) (putMVar writeLock)
  $ const
  $ withImmediateTransaction conn ioacts

