{-# LANGUAGE OverloadedStrings #-}

-- put individual low level alteration commands in each of the type files, but put high level db interaction endpoints here
module DbManager where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Data.Either
import           Data.Map as Map (Map)
import qualified Data.Map as Map
import           Data.Map.Merge.Lazy
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Types.Errors
import           TextShow
import           Types.SchemaTypes
import           Types.SchemaOps
import           Types.DBIO

data Value = Value { lovelace :: Int, tokens :: Map Text Int }
  deriving (Show)

instance Eq Value where
  (==) (Value l1 t1) (Value l2 t2) =
    l1 == l2 && diffEqZero t1 t2 && diffEqZero t2 t1
    where
      diffEqZero tokens1 tokens2 = all (== 0)
        $ Map.elems
        $ Map.differenceWith
          (\a b -> if a == b
                   then Nothing
                   else Just a)
          tokens1
          tokens2

instance Semigroup Value where
  (<>) (Value l1 t1) (Value l2 t2) = Value (l1 + l2)
    $ merge preserveMissing preserveMissing (zipWithMatched $ const (+)) t1 t2

instance Monoid Value where
  mempty = Value 0 Map.empty

getUserBalance :: DiscordId -> Connection -> IO (Either OperationError Value)
getUserBalance did conn = runExceptT
  $ do
    rlovelace <- lift
      (query conn "SELECT lovelace_balance FROM user WHERE did=(?)" (Only did)
         :: IO [Only Int])
    when (length rlovelace /= 1)
      $ throwError
      $ ConditionFailureError
      $ "Could not find user record in getUserBalance: " <> showt did
    let (Only lovelaceamt) = head rlovelace
    rtokens <- lift
      (query
         conn
         "SELECT token_id, amount FROM user_balance WHERE user_did=(?)"
         (Only did) :: IO [(Text, Int)])
    pure $ Value lovelaceamt $ Map.fromList rtokens

modifyUserBalance :: MVar ()
                  -> DiscordId
                  -> Value
                  -> Connection
                  -> IO (Either OperationError ())
modifyUserBalance writeLock did diffVal conn = runExceptT
  $ do
    let (Value diffLovelace diffTokens) = diffVal
        nonzeroDiffTokens = Map.filter (/= 0) diffTokens
    (Value _ eTok) <- addUserIfNotExists writeLock did conn
    transaction <- writeTransact writeLock conn
      $ do
        modifyLovelace False diffLovelace did conn
        modifyTokens False nonzeroDiffTokens did eTok conn
    case transaction of
      Left e  -> throwError
        (SQLiteError
           ("Could not modifyUserBalance: \nDid: "
            <> showt did
            <> "\nValue: "
            <> T.pack (show diffVal)
            <> "; transaction failed.")
           e)
      Right _ -> pure ()

transferUserBalance :: MVar ()
                    -> (DiscordId, Value)
                    -> DiscordId
                    -> Connection
                    -> IO (Either OperationError ())
transferUserBalance writeLock (sourcedid, diffVal) destdid conn = runExceptT
  $ do
    let (Value diffLovelace diffTokens) = diffVal
        nonzeroDiffTokens = Map.filter (/= 0) diffTokens
    existingSourceBalance <- lift $ getUserBalance sourcedid conn
    when (isLeft existingSourceBalance)
      $ throwError
      $ ConditionFailureError
        ("Could not transferUserBalance: source user "
         <> showt sourcedid
         <> " does not exist.")
      <> fromLeft mempty existingSourceBalance
    let (Value sourceeVal sourceeTok) = fromRight mempty existingSourceBalance
    (Value _ desteTok) <- addUserIfNotExists writeLock destdid conn
    transaction <- lift
      $ try
      $ bracket (takeMVar writeLock) (putMVar writeLock)
      $ const
      $ withImmediateTransaction conn
      $ do
        modifyLovelace True diffLovelace sourcedid conn
        modifyLovelace False diffLovelace destdid conn
        modifyTokens True nonzeroDiffTokens sourcedid sourceeTok conn
        modifyTokens False nonzeroDiffTokens destdid desteTok conn
    case transaction of
      Left e  -> throwError
        (SQLiteError
           ("Could not transferUserBalance: transaction failed.\nSource did: "
            <> showt sourcedid
            <> "\nDest did: "
            <> showt destdid
            <> "\nValue: "
            <> T.pack (show diffVal))
           e)
      Right _ -> pure ()

-- adds user if doesn't exist, returning either existing value or value after add
addUserIfNotExists
  :: MVar () -> DiscordId -> Connection -> ExceptT OperationError IO Value
addUserIfNotExists writeLock did conn = do
  existingBalance <- lift $ getUserBalance did conn
  when (isLeft existingBalance)
    $ do
      r <- lift $ addNewUser writeLock did conn
      case r of
        Left e  -> throwError
          (ConditionFailureError
             ("Could not modifyUserBalance on did "
              <> showt did
              <> ": existing record could not be found, and insert operation failed.")
           <> e
           <> fromLeft mempty existingBalance)
        Right _ -> pure ()
  case existingBalance of
    Left _     -> pure mempty
    Right eBal -> pure eBal

addNewUser
  :: MVar () -> DiscordId -> Connection -> IO (Either OperationError ())
addNewUser writeLock did conn = runExceptT
  $ do
    r <- writeTransact writeLock conn
      $ execute conn "INSERT INTO user (did) VALUES (?)" (Only did)
    case r of
      Left e  -> throwError
        $ SQLiteError ("Failed to add new user: " <> showt did) e
      Right _ -> pure ()
