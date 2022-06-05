{-# LANGUAGE DeriveGeneric #-}

module Types.SchemaTypes where

import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple
import           GHC.Generics
import           Data.Aeson

data Token = Token { id :: Text, name :: Text, decimals :: Int }
  deriving (Show, Generic)

instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Token

instance FromRow Token where
  fromRow = Token <$> field <*> field <*> field

instance ToRow Token where
  toRow (Token id_ name_ decimals_) = toRow (id_, name_, decimals_)

data TokenAlias = TokenAlias { alias :: Text, assetid :: Int }
  deriving (Eq, Show)

instance FromRow TokenAlias where
  fromRow = TokenAlias <$> field <*> field

instance ToRow TokenAlias where
  toRow (TokenAlias alias_ assetid_) = toRow (alias_, assetid_)

type DiscordId = Int

data UserRecord = UserRecord { did :: DiscordId, lovelace_balance :: Int }
  deriving (Eq, Show)

instance FromRow UserRecord where
  fromRow = UserRecord <$> field <*> field

instance ToRow UserRecord where
  toRow (UserRecord did_ lovelace_balance_) = toRow (did_, lovelace_balance_)

data UserBalance =
  UserBalance { token_id :: Text, user_did :: Int, amount :: Int }
  deriving (Eq, Show)

instance FromRow UserBalance where
  fromRow = UserBalance <$> field <*> field <*> field

instance ToRow UserBalance where
  toRow (UserBalance token_id_ user_did_ amount_) =
    toRow (token_id_, user_did_, amount_)

