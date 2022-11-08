{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- put individual low level alteration commands in each of the type files, but put high level db interaction endpoints here
module TipBackend where

import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Data.Aeson
import Data.Either
import Data.Map as Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy
import Data.OpenApi
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import GHC.Generics
import TextShow
import Types.DBIO
import Types.Errors
import Types.SchemaOps
import Types.SchemaTypes

data CValue = CValue {lovelace :: Int, tokens :: Map Text Int}
  deriving (Show, Generic)

instance Eq CValue where
  (==) (CValue l1 t1) (CValue l2 t2) =
    l1 == l2 && diffEqZero t1 t2 && diffEqZero t2 t1
    where
      diffEqZero tokens1 tokens2 =
        all (== 0) $
          Map.elems $
            Map.differenceWith
              ( \a b ->
                  if a == b
                    then Nothing
                    else Just a
              )
              tokens1
              tokens2

instance ToJSON CValue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CValue

instance Semigroup CValue where
  (<>) (CValue l1 t1) (CValue l2 t2) =
    CValue (l1 + l2) $
      merge preserveMissing preserveMissing (zipWithMatched $ const (+)) t1 t2

instance Monoid CValue where
  mempty = CValue 0 Map.empty

instance ToSchema CValue

data UserCValue = UserCValue {udid :: DiscordId, cvalue :: CValue} deriving (Show, Eq, Generic)

instance ToJSON UserCValue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserCValue

instance ToSchema UserCValue

listUserRecords :: Connection -> IO (Either OperationError [UserRecord])
listUserRecords conn =
  handle
    (pure . Left . SQLiteError "Failed to get alias assigned addresses")
    (Right <$> query_ conn "SELECT * FROM user")

listBackendTokens :: Connection -> IO (Either OperationError [Token])
listBackendTokens conn =
  runExceptT $
    lift (query_ conn "SELECT * FROM tokens")

addBackendToken ::
  Token -> MVar () -> Connection -> IO (Either OperationError ())
addBackendToken tok writeLock conn =
  runExceptT $
    either handleAddTokenError pure
      =<< writeTransact
        writeLock
        conn
        ( execute
            conn
            "INSERT INTO tokens VALUES (?,?,?)"
            tok
        )
  where
    handleAddTokenError :: SQLError -> ExceptT OperationError IO ()
    handleAddTokenError =
      throwError
        . SQLiteError ("Failed to add new token: " <> T.pack (show tok))

editBackendToken ::
  Token -> MVar () -> Connection -> IO (Either OperationError ())
editBackendToken tok writeLock conn =
  runExceptT $
    either handleEditTokenError pure
      =<< writeTransact
        writeLock
        conn
        ( execute
            conn
            "UPDATE tokens SET name = (?), decimals = (?) WHERE id = (?)"
            tok
        )
  where
    handleEditTokenError :: SQLError -> ExceptT OperationError IO ()
    handleEditTokenError =
      throwError
        . SQLiteError ("Failed to edit token: " <> T.pack (show tok))

getUserRecord :: DiscordId -> Connection -> IO (Either OperationError UserRecord)
getUserRecord did conn = handle
  (pure . Left . SQLiteError ("Failed to get UserRecord: " <> showt did))
  $ do
    r <- query conn "SELECT * FROM user WHERE did=(?)" (Only did)
    case length r of
      1 -> pure $ Right $ head r
      _ -> pure $ Left $ ConditionFailureError "User does not exist."

addNewUser ::
  DiscordId -> Text -> MVar () -> Connection -> IO (Either OperationError ())
addNewUser did c_addr writeLock conn =
  runExceptT $
    either handleTxFailure pure
      =<< writeTransact
        writeLock
        conn
        ( execute conn "INSERT INTO user (did, c_addr) VALUES (?,?)" (did, c_addr)
        )
  where
    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure =
      throwError
        . SQLiteError ("Failed to add new user: " <> showt did)

addProcessedTx :: Text -> MVar () -> Connection -> IO (Either OperationError ())
addProcessedTx proctxid writeLock conn =
  runExceptT $
    either (throwError . SQLiteError "Could not add processed tx - tx has already been processed") pure
      =<< writeTransact
        writeLock
        conn
        ( execute conn "INSERT INTO processed_txs (txid) VALUES (?)" (Only proctxid)
        )

getProcessedTxs :: Connection -> IO (Either OperationError [Text])
getProcessedTxs conn =
  either (pure . Left) (pure . Right . map txid)
    =<< handle
      (pure . Left . SQLiteError "Failed to get processed txs.")
      (Right <$> query_ conn "SELECT * FROM processed_txs")

getUserBalance :: DiscordId -> Connection -> IO (Either OperationError CValue)
getUserBalance did conn = runExceptT $ do
  rlovelace <-
    lift
      ( query conn "SELECT lovelace_balance FROM user WHERE did=(?)" (Only did) ::
          IO [Only Int]
      )
  when (length rlovelace /= 1) $
    throwError $
      ConditionFailureError $
        "Could not find user record in getUserBalance: " <> showt did
  rtokens <-
    lift
      ( query
          conn
          "SELECT token_id, amount FROM user_balance WHERE user_did=(?)"
          (Only did)
      )
  let (Only lovelaceamt) = head rlovelace
  pure $ CValue lovelaceamt $ Map.fromList rtokens

modifyUserBalance ::
  DiscordId ->
  CValue ->
  MVar () ->
  Connection ->
  IO (Either OperationError ())
modifyUserBalance did diffVal writeLock conn = runExceptT $ do
  let (CValue diffLovelace diffTokens) = diffVal
      nonzeroDiffTokens = Map.filter (/= 0) diffTokens
  lift (getUserBalance did conn)
    >>= either
      handleNonExistentDest
      ( \(CValue _ eTok) ->
          either handleTxFailure pure
            =<< writeTransact
              writeLock
              conn
              ( do
                  modifyLovelace False diffLovelace did conn
                  modifyTokens False nonzeroDiffTokens did eTok conn
              )
      )
  where
    handleNonExistentDest :: OperationError -> ExceptT OperationError IO ()
    handleNonExistentDest e =
      throwError $
        ConditionFailureError
          ( "Could not transferUserBalance: dest user "
              <> showt did
              <> " does not exist."
          )
          <> e

    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure =
      throwError
        . SQLiteError
          ( "Could not modifyUserBalance: \nDid: "
              <> showt did
              <> "\nValue: "
              <> T.pack (show diffVal)
              <> "; tx failed."
          )

transferUserBalance ::
  DiscordId ->
  [UserCValue] ->
  MVar () ->
  Connection ->
  IO (Either OperationError [UserCValue])
transferUserBalance sourcedid ldestcvalue writeLock conn = runExceptT $ do
  desteithercvals <- mapM (\ucv -> lift $ getUserBalance (udid ucv) conn) ldestcvalue
  let (desterrs, destcvals) = partitionEithers desteithercvals
      desteToks = map tokens destcvals
  unless (null desterrs) $
    throwError $
      ConditionFailureError "Could not transferMult: dest user does not exist. "
        <> mconcat desterrs
  lift (getUserBalance sourcedid conn)
    >>= either
      handleNonExistentSource
      ( \(CValue _ sourceeTok) -> do
          r <-
            writeTransact
              writeLock
              conn
              $ zipWithM_
                ( \destcval desteTok ->
                    singleTransfer
                      sourcedid
                      destcval
                      sourceeTok
                      desteTok
                      conn
                )
                ldestcvalue
                desteToks
          either handleTxFailure pure r
      )
  let afterdids = (sourcedid :) $ map udid ldestcvalue
  newbalances <- mapM (\adid -> lift $ getUserBalance adid conn) afterdids
  let (aftererrs, aftercvals) = partitionEithers newbalances
  pure $ zipWith UserCValue afterdids aftercvals
  where
    singleTransfer :: DiscordId -> UserCValue -> Map Text Int -> Map Text Int -> Connection -> IO ()
    singleTransfer sourcedid (UserCValue destdid diffVal) sourceeTok desteTok conn =
      do
        let (CValue diffLovelace diffTokens) = diffVal
            nonzeroDiffTokens = Map.filter (/= 0) diffTokens
        modifyLovelace True diffLovelace sourcedid conn
        modifyLovelace False diffLovelace destdid conn
        modifyTokens True nonzeroDiffTokens sourcedid sourceeTok conn
        modifyTokens False nonzeroDiffTokens destdid desteTok conn

    handleNonExistentSource :: OperationError -> ExceptT OperationError IO ()
    handleNonExistentSource e =
      throwError $
        ConditionFailureError
          ( "Could not transferMult: source user "
              <> showt sourcedid
              <> " does not exist."
          )
          <> e

    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure =
      throwError
        . SQLiteError
          ( "Could not transferMult: tx failed.\nSource did: "
              <> showt sourcedid
          )

-- Make sure that these aliases are capitalised!
-- aliases are case insensitive and always stored as capitals
addAlias ::
  Text -> Text -> MVar () -> Connection -> IO (Either OperationError ())
addAlias al aid writeLock conn =
  runExceptT $
    either handleTxFailure pure
      =<< writeTransact
        writeLock
        conn
        ( execute conn "INSERT INTO aliases VALUES (?,?)" (T.toUpper al, aid)
        )
  where
    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure = throwError . SQLiteError ("Failed to add alias: " <> T.pack (show al))

getAlias :: Text -> Connection -> IO (Either OperationError TokenAlias)
getAlias al conn = handle
  (pure . Left . SQLiteError ("Failed to get alias: " <> al))
  $ do
    r <- query conn "SELECT * FROM aliases WHERE alias=(?)" (Only $ T.toUpper al)
    case length r of
      1 -> pure $ Right $ head r
      _ -> pure $ Left $ ConditionFailureError "Alias does not exist."

-- get all the aliases for a particular token name
getTokenAliases ::
  Text -> Connection -> IO (Either OperationError [TokenAlias])
getTokenAliases aid conn =
  handle
    (pure . Left . SQLiteError ("Failed to get alias: " <> aid))
    $ Right <$> query conn "SELECT * FROM aliases WHERE assetid=(?)" (Only aid)
