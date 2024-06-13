{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Insert
  ( Insertable(..)
  , insert
  , insertMany
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Prelude

-- | Used to generate the table's insert statement.
class Insertable a where
  insertStmt :: Sqlite.Query

insert :: forall a. (Sqlite.ToRow a, Insertable a) => String -> a -> IO ()
insert dbName info = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute conn (insertStmt @a) info

insertMany :: forall a. (Sqlite.ToRow a, Insertable a) => String -> [a] -> IO ()
insertMany dbName info = Sqlite.withConnection dbName $ \conn ->
  Sqlite.executeMany conn (insertStmt @a) info
