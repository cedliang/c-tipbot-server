PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
BEGIN TRANSACTION;
CREATE TABLE tokens (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  decimals integer NOT NULL CHECK(decimals >= 0) default 0
);
CREATE TABLE aliases (
  alias TEXT PRIMARY KEY,
  assetid TEXT NOT NULL,
  FOREIGN KEY (assetid) REFERENCES tokens(id)
);
CREATE TABLE user (
  did INTEGER PRIMARY KEY CHECK(did >= 0),
  lovelace_balance INTEGER NOT NULL CHECK(lovelace_balance >= 0) DEFAULT 0,
  c_addr TEXT NOT NULL UNIQUE
);
CREATE TABLE user_balance (
  token_id TEXT NOT NULL,
  user_did INT NOT NULL,
  amount INT NOT NULL,
  FOREIGN KEY (token_id) REFERENCES tokens(id),
  FOREIGN KEY (user_did) REFERENCES user(did),
  CHECK(amount >= 0),
  UNIQUE(token_id, user_did)
);
COMMIT;