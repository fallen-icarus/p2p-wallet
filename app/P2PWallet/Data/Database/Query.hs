{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Query
  ( Queryable(..)
  , query
  , queryRaw
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Data.Database.Pattern
import P2PWallet.Prelude

-- | Used to generate the table's query statement.
class Queryable a where
  queryStmt :: Sqlite.Query

query :: forall a. (Sqlite.FromRow a, Queryable a) => String -> [Pattern] -> IO [a]
query dbName ps = Sqlite.withConnection dbName $ \conn ->
  Sqlite.query_ conn $ queryStmt @a <> patternToSql ps

queryRaw :: forall a. (Sqlite.FromRow a) => String -> Sqlite.Query -> IO [a]
queryRaw dbName q = Sqlite.withConnection dbName $ \conn ->
  Sqlite.query_ conn q
