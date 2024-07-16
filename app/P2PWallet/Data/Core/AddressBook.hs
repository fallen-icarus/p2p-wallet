{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.AddressBook where

import Database.SQLite.Simple (ToRow(..),FromRow(..))

import P2PWallet.Data.Core.Internal
import P2PWallet.Database
import P2PWallet.Prelude

data AddressEntry = AddressEntry
  { profileId :: ProfileId
  , contactId :: ContactId
  , alias :: Text
  , paymentAddress :: PaymentAddress
  } deriving (Show,Eq,Generic,FromRow,ToRow)

makeFieldLabelsNoPrefix ''AddressEntry

instance Default AddressEntry where
  def = AddressEntry
    { profileId = 0
    , contactId = 0
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
        , "contact_id INTEGER PRIMARY KEY"
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
        , "contact_id"
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

verifyNewAddressEntry 
  :: NewAddressEntry 
  -> Network
  -> ProfileId 
  -> ContactId 
  -> [AddressEntry] 
  -> Either Text AddressEntry
verifyNewAddressEntry NewAddressEntry{..} network profileId contactId book = do
  -- Verify that the alias is not already being used.
  maybeToLeft () $ "This alias is already being used for another address." <$
    find (\a -> a ^. #alias == alias) book

  -- Verify the address is a real payment address.
  addr <- parsePaymentAddress network paymentAddress

  return $ AddressEntry
    { profileId = profileId
    , contactId = contactId
    , alias = alias
    , paymentAddress = addr
    }
