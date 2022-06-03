{-# LANGUAGE OverloadedStrings #-}

module Types.SchemaOps where

import           Types.SchemaTypes
import           Control.Concurrent.MVar
import           Types.DBIO
import           Types.Errors
import           Database.SQLite.Simple
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Except
import           Control.Exception (bracket, handle)
import           Data.Map as Map (Map)
import qualified Data.Map as Map

-- Make sure that these aliases are capitalised!
addAlias
  :: MVar () -> TokenAlias -> Connection -> IO (Either OperationError ())
addAlias writeLock alias conn = runExceptT
  $ do
    tx <- writeTransact writeLock conn
      $ execute conn "INSERT INTO alias VALUES (?)" alias
    throwOnLeft handleTxFailure tx pure
  where
    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure = throwError
      . SQLiteError ("Failed to add alias: " <> T.pack (show alias))

getAlias :: Text -> Connection -> IO (Either OperationError TokenAlias)
getAlias alias conn = handle
  (pure . Left . SQLiteError ("Failed to get alias: " <> alias))
  $ do
    r <- query
      conn
      "SELECT * FROM alias WHERE alias=(?)"
      (Only $ T.toUpper alias)
    case length r of
      1 -> pure $ Right $ head r
      _ -> pure $ Left $ ConditionFailureError "Alias does not exist."

-- low level, with no bracketing or error handling. Should never be called directly by any user facing element.
modifyLovelace :: Bool -> Int -> DiscordId -> Connection -> IO ()
modifyLovelace deductInstead diffLovelace did conn =
  let changeLovelace = if deductInstead
                       then (-1) * diffLovelace
                       else diffLovelace
  in when (changeLovelace /= 0)
     $ execute
       conn
       "UPDATE user SET lovelace_balance=lovelace_balance+(?) WHERE did=(?)"
       (changeLovelace, did)

-- low level, with no bracketing or error handling. Should never be called directly by any user facing element.
modifyTokens
  :: Bool -> Map Text Int -> DiscordId -> Map Text Int -> Connection -> IO ()
modifyTokens deductInstead diffTokens did usereTok conn =
  let changeTokens = if deductInstead
                     then Map.map (* (-1)) diffTokens
                     else diffTokens
  in unless (null changeTokens)
     $ do
       let listDiffTokens = Map.toList changeTokens
       forM_ listDiffTokens
         $ \(tokname, amount)
         -> if Map.member tokname usereTok
            then execute
              conn
              "UPDATE user_balance SET amount=amount+(?) WHERE user_did=(?) AND token_id=(?)"
              (amount, did, tokname)
            else execute
              conn
              "INSERT INTO user_balance VALUES (?,?,?)"
              (tokname, did, amount)
