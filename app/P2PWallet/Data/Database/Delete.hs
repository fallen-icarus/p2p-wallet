{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Delete
  ( Deletable(..)
  , delete
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Data.Database.Pattern
import P2PWallet.Prelude

-- | Used to generate the table's delete statement.
class Deletable a where
  deleteStmt :: Sqlite.Query

delete :: forall a. (Sqlite.FromRow a, Deletable a) => String -> [Pattern] -> IO ()
delete dbName ps = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn $ deleteStmt @a <> patternToSql ps
