{-# LANGUAGE AllowAmbiguousTypes #-}

module P2PWallet.Data.Database.Delete
  ( delete
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Prelude

delete :: String -> Sqlite.Query -> IO ()
delete dbName deleteStmt = Sqlite.withConnection dbName $ \conn ->
  Sqlite.execute_ conn deleteStmt
