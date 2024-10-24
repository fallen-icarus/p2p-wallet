{-# LANGUAGE AllowAmbiguousTypes #-}

{-

This module only contains the basic building blocks for interacting with the sqlite database. The
schemas for each table are defined in the same modules as the corresponding types, and the functions
for accessing the database are defined in `P2PWallet.Actions.Database`.

-}
module P2PWallet.Database
  ( -- * Creating Tables
    TableName(..)
  , Creatable(..)
  , create

    -- * Deleting Entries
  , delete

    -- * Updating Entries
  , update

    -- * Inserting Entries
  , Insertable(..)
  , insert
  , insertMany

    -- * Querying the Database
  , query

    -- * Re-Exports
  , Sqlite.Query(..)
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Prelude

-------------------------------------------------
-- Creating Tables
-------------------------------------------------
-- | Used to generate the table's name.
class TableName a where
  tableName :: Text

-- | Used to generate the table's creation statement.
class Creatable a where
  createStmt :: Sqlite.Query

create :: forall a. (Creatable a) => String -> IO ()
create dbName = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn $ createStmt @a

-------------------------------------------------
-- Deleting Entries
-------------------------------------------------
delete :: String -> Sqlite.Query -> IO ()
delete dbName deleteStmt = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn deleteStmt

-------------------------------------------------
-- Updating Entries
-------------------------------------------------
update :: String -> Sqlite.Query -> IO ()
update dbName updateStmt = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn updateStmt

-------------------------------------------------
-- Inserting Entries
-------------------------------------------------
-- | Used to generate the table's insert statement.
class Insertable a where
  insertStmt :: Sqlite.Query

insert :: forall a. (Sqlite.ToRow a, Insertable a) => String -> a -> IO ()
insert dbName info = Sqlite.withConnection dbName $ \conn -> do
  Sqlite.execute conn (insertStmt @a) info

insertMany :: forall a. (Sqlite.ToRow a, Insertable a) => String -> [a] -> IO ()
insertMany dbName info = Sqlite.withConnection dbName $ \conn ->
  Sqlite.executeMany conn (insertStmt @a) info

-------------------------------------------------
-- Querying the Database
-------------------------------------------------
query :: forall a. (Sqlite.FromRow a) => String -> Sqlite.Query -> IO [a]
query dbName queryStmt = Sqlite.withConnection dbName $ \conn ->
  Sqlite.query_ conn queryStmt
