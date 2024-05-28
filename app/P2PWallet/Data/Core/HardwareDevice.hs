{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.HardwareDevice where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-------------------------------------------------
-- Supported Device Derivations
-------------------------------------------------
data HwDevice
  = Ledger
  | Trezor
  | TrezorLegacy
  deriving (Show,Eq,Ord,Enum)

makePrisms ''HwDevice

instance ToField HwDevice where
  toField = toField . showHwDevice

instance FromField HwDevice where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "failed to parse HwDevice") Ok $ readHwDevice t
  fromField f = returnError ConversionFailed f "need a text"

readHwDevice :: Text -> Maybe HwDevice
readHwDevice = \case
  "ledger" -> Just Ledger
  "trezor" -> Just Trezor
  "trezor-legacy" -> Just TrezorLegacy
  _ -> Nothing

showHwDevice :: HwDevice -> Text
showHwDevice Ledger = "ledger"
showHwDevice TrezorLegacy = "trezor-legacy"
showHwDevice Trezor = "trezor"

displayHwDevice :: HwDevice -> Text
displayHwDevice Ledger = "Ledger"
displayHwDevice TrezorLegacy = "Legacy Trezor"
displayHwDevice Trezor = "Trezor"

-- | Used for a dropdown menu.
supportedDevices :: [HwDevice]
supportedDevices = enumFrom Ledger
