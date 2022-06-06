{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- put individual low level alteration commands in each of the type files, but put high level db interaction endpoints here
module TipBackend where

import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Data.Aeson
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

listBackendTokens :: Connection -> IO (Either OperationError [Token])
listBackendTokens conn =
    runExceptT $
        lift (query_ conn "SELECT * FROM tokens" :: IO [Token])

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
                (Only did) ::
                IO [(Text, Int)]
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
    (CValue _ eTok) <- addUserIfNotExists did writeLock conn
    either handleTxFailure pure
        =<< writeTransact
            writeLock
            conn
            ( do
                modifyLovelace False diffLovelace did conn
                modifyTokens False nonzeroDiffTokens did eTok conn
            )
  where
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
    (DiscordId, CValue) ->
    DiscordId ->
    MVar () ->
    Connection ->
    IO (Either OperationError ())
transferUserBalance (sourcedid, diffVal) destdid writeLock conn = runExceptT $ do
    let (CValue diffLovelace diffTokens) = diffVal
        nonzeroDiffTokens = Map.filter (/= 0) diffTokens
    either
        handleNonExistentSource
        (existentSourceAction diffLovelace nonzeroDiffTokens)
        =<< lift (getUserBalance sourcedid conn)
  where
    existentSourceAction :: Int -> Map Text Int -> CValue -> ExceptT OperationError IO ()
    existentSourceAction diffLovelace nonzeroDiffTokens (CValue sourceeVal sourceeTok) = do
        (CValue _ desteTok) <- addUserIfNotExists destdid writeLock conn
        either handleTxFailure pure
            =<< writeTransact
                writeLock
                conn
                ( do
                    modifyLovelace True diffLovelace sourcedid conn
                    modifyLovelace False diffLovelace destdid conn
                    modifyTokens True nonzeroDiffTokens sourcedid sourceeTok conn
                    modifyTokens False nonzeroDiffTokens destdid desteTok conn
                )

    handleNonExistentSource :: OperationError -> ExceptT OperationError IO ()
    handleNonExistentSource e =
        throwError $
            ConditionFailureError
                ( "Could not transferUserBalance: source user "
                    <> showt sourcedid
                    <> " does not exist."
                )
                <> e

    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure =
        throwError
            . SQLiteError
                ( "Could not transferUserBalance: tx failed.\nSource did: "
                    <> showt sourcedid
                    <> "\nDest did: "
                    <> showt destdid
                    <> "\nValue: "
                    <> T.pack (show diffVal)
                )

-- adds user if doesn't exist, returning either existing value or value after add
addUserIfNotExists ::
    DiscordId -> MVar () -> Connection -> ExceptT OperationError IO CValue
addUserIfNotExists did writeLock conn =
    either handleNotExists pure =<< lift (getUserBalance did conn)
  where
    handleNotExists :: OperationError -> ExceptT OperationError IO CValue
    handleNotExists eBal =
        either (handleAddUser eBal) (const $ pure mempty) =<< lift (addNewUser did writeLock conn)

    handleAddUser ::
        OperationError -> OperationError -> ExceptT OperationError IO CValue
    handleAddUser eBal e =
        throwError $
            ConditionFailureError
                ( "Could not modifyUserBalance on did "
                    <> showt did
                    <> ": existing record could not be found, and insert operation failed."
                )
                <> e
                <> eBal

addNewUser ::
    DiscordId -> MVar () -> Connection -> IO (Either OperationError ())
addNewUser did writeLock conn =
    runExceptT $
        either handleTxFailure pure
            =<< writeTransact
                writeLock
                conn
                ( execute conn "INSERT INTO user (did) VALUES (?)" (Only did)
                )
  where
    handleTxFailure :: SQLError -> ExceptT OperationError IO ()
    handleTxFailure =
        throwError
            . SQLiteError ("Failed to add new user: " <> showt did)

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
