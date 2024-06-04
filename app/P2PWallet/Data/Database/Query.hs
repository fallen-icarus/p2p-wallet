{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Query
  ( query
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Prelude

query :: forall a. (Sqlite.FromRow a) => String -> Sqlite.Query -> IO [a]
query dbName queryStmt = Sqlite.withConnection dbName $ \conn ->
  Sqlite.query_ conn queryStmt
