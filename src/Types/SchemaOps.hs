{-# LANGUAGE OverloadedStrings #-}

module Types.SchemaOps where

import Control.Monad.Except
import Data.Map as Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Database.SQLite.Simple
import Types.SchemaTypes

-- low level, with no bracketing or error handling. Should never be called directly by any user facing element.
modifyLovelace :: Bool -> Int -> DiscordId -> Connection -> IO ()
modifyLovelace deductInstead diffLovelace did conn =
  when (changeLovelace /= 0) $
    execute
      conn
      "UPDATE user SET lovelace_balance=lovelace_balance+(?) WHERE did=(?)"
      (changeLovelace, did)
  where
    changeLovelace =
      if deductInstead
        then (-1) * diffLovelace
        else diffLovelace

-- low level, with no bracketing or error handling. Should never be called directly by any user facing element.
modifyTokens ::
  Bool -> Map Text Int -> DiscordId -> Map Text Int -> Connection -> IO ()
modifyTokens deductInstead diffTokens did usereTok conn =
  unless (null changeTokens) $
    forM_ (Map.toList changeTokens) $
      \(tokname, amount) ->
        if Map.member tokname usereTok
          then
            execute
              conn
              "UPDATE user_balance SET amount=amount+(?) WHERE user_did=(?) AND token_id=(?)"
              (amount, did, tokname)
          else
            execute
              conn
              "INSERT INTO user_balance VALUES (?,?,?)"
              (tokname, did, amount)
  where
    changeTokens =
      if deductInstead
        then Map.map (* (-1)) diffTokens
        else diffTokens