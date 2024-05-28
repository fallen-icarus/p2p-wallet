{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Create
  ( Creatable(..)
  , create
  , TableName(..)
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Prelude

-- | Used to generate the table's creation statement.
class Creatable a where
  createStmt :: Sqlite.Query

create :: forall a. (Creatable a) => String -> IO ()
create dbName = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn $ createStmt @a

-- | Used to generate the table's name.
class TableName a where
  tableName :: Text
