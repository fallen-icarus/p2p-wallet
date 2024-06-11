{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AddressBook where

import Database.SQLite.Simple (ToRow(..),FromRow(..))

import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Prelude

data AddressEntry = AddressEntry
  { profileId :: ProfileId
  , addressEntryId :: AddressEntryId
  , alias :: Text
  , paymentAddress :: PaymentAddress
  } deriving (Show,Eq,Generic,FromRow,ToRow)

makeFieldLabelsNoPrefix ''AddressEntry

instance Default AddressEntry where
  def = AddressEntry
    { profileId = 0
    , addressEntryId = 0
    , alias = ""
    , paymentAddress = ""
    }

instance TableName AddressEntry where
  tableName = "address_book"

instance Creatable AddressEntry where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @AddressEntry
    , "("
    , unwords $ intersperse ","
        [ "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "address_entry_id INTEGER PRIMARY KEY"
        , "alias TEXT NOT NULL"
        , "payment_address TEXT NOT NULL"
        , "UNIQUE(profile_id,alias)"
        ]
    , ");"
    ]

instance Insertable AddressEntry where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @AddressEntry
    , "("
    , unwords $ intersperse ","
        [ "profile_id"
        , "address_entry_id"
        , "alias"
        , "payment_address"
        ]
    , ")"
    , "VALUES (?,?,?,?);"
    ]


-------------------------------------------------
-- New Address Entry
-------------------------------------------------
data NewAddressEntry = NewAddressEntry
  { alias :: Text
  , paymentAddress :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewAddressEntry

instance Default NewAddressEntry where
  def = NewAddressEntry
    { alias = ""
    , paymentAddress = ""
    }
