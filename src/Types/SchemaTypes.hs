{-# LANGUAGE DeriveGeneric #-}

module Types.SchemaTypes where

import Data.Aeson
import Data.OpenApi
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import GHC.Generics

data Token = Token {id :: Text, name :: Text, decimals :: Int}
  deriving (Show, Generic)

instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Token

instance FromRow Token where
  fromRow = Token <$> field <*> field <*> field

instance ToRow Token where
  toRow (Token id_ name_ decimals_) = toRow (id_, name_, decimals_)

instance ToSchema Token

data TokenAlias = TokenAlias {alias :: Text, assetid :: Text}
  deriving (Eq, Show, Generic)

instance FromRow TokenAlias where
  fromRow = TokenAlias <$> field <*> field

instance ToJSON TokenAlias where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TokenAlias

instance ToRow TokenAlias where
  toRow (TokenAlias alias_ assetid_) = toRow (alias_, assetid_)

instance ToSchema TokenAlias

type DiscordId = Int

data UserRecord = UserRecord {did :: DiscordId, lovelace_balance :: Int, c_addr :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON UserRecord where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserRecord

instance FromRow UserRecord where
  fromRow = UserRecord <$> field <*> field <*> field

instance ToRow UserRecord where
  toRow (UserRecord did_ lovelace_balance_ c_addr_) = toRow (did_, lovelace_balance_, c_addr_)

instance ToSchema UserRecord

data UserBalance = UserBalance {token_id :: Text, user_did :: Int, amount :: Int}
  deriving (Eq, Show, Generic)

instance FromRow UserBalance where
  fromRow = UserBalance <$> field <*> field <*> field

instance ToRow UserBalance where
  toRow (UserBalance token_id_ user_did_ amount_) =
    toRow (token_id_, user_did_, amount_)

instance ToSchema UserBalance