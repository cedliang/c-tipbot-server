{-# LANGUAGE OverloadedStrings #-}

module Types.Errors where

import Control.Exception (Exception)
import Data.Text (Text)
import Database.SQLite.Simple (SQLError)

data OperationError
  = ConditionFailureError Text
  | SQLiteError Text SQLError
  | ErrorChain OperationError OperationError
  deriving (Eq, Show)

instance Exception OperationError

instance Semigroup OperationError where
  (<>) = ErrorChain

instance Monoid OperationError where
  mempty =
    ConditionFailureError
      "Mempty default that should not appear due to preconditions."
