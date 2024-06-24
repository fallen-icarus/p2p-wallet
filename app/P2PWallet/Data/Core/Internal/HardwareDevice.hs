{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.HardwareDevice where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-------------------------------------------------
-- Supported Device Derivations
-------------------------------------------------
data HardwareDevice
  = Ledger
  | Trezor
  | TrezorLegacy
  deriving (Show,Eq,Ord,Enum,Read)

makePrisms ''HardwareDevice

instance Display HardwareDevice where
  display Ledger = "Ledger"
  display Trezor = "Trezor"
  display TrezorLegacy = "Legacy Trezor"

instance ToField HardwareDevice where
  toField = toField @Text . show

instance FromField HardwareDevice where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "failed to parse HardwareDevice") Ok $ 
      parseHardwareDevice t
  fromField f = returnError ConversionFailed f "need a text"

parseHardwareDevice :: Text -> Maybe HardwareDevice
parseHardwareDevice = readMaybe . toString

-- | Used for a dropdown menu.
supportedDevices :: [HardwareDevice]
supportedDevices = enumFrom Ledger
