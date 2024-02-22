{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-

A `Profile` is related to the derivation path for hardware keys in that it is the account index 
combined with an alias. The derivation path is outlined as:

@
    m / purpose' / coin_type' / account' / chain / address_index
@

When signing transactions with hardware wallets, all keys must be for the same account index.
Therefore, the profile name can be thought of the key family name. Every `Profile` will get
their own backup file and wallets. The app will only have one profile loaded at a given time but
users can easily switch between profiles from within the app.

-}
module P2PWallet.Data.Core.Profile where

import Data.Aeson

import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Prelude

data HwDevice
  = Ledger
  | Trezor
  | TrezorLegacy
  deriving (Show,Eq,Ord,Enum)

instance ToJSON HwDevice where
  toJSON Ledger = "ledger"
  toJSON TrezorLegacy = "trezor-legacy"
  toJSON Trezor = "trezor"

instance FromJSON HwDevice where
  parseJSON = withText "HwDevice" $ \case
    "ledger" -> pure Ledger
    "trezor" -> pure Trezor
    "trezor-legacy" -> pure TrezorLegacy
    _ -> mzero

showHwDevice :: HwDevice -> Text
showHwDevice Ledger = "Ledger"
showHwDevice TrezorLegacy = "Legacy Trezor"
showHwDevice Trezor = "Trezor"

-- | Used for a dropdown menu.
supportedDevices :: [HwDevice]
supportedDevices = enumFrom Ledger

-- | A type represnting a user profile.
data Profile = Profile
  { _accountIndex :: AccountIndex -- ^ The key family associated with this profile.
  , _alias :: Text -- ^ The user friendly name for this profile.
  , _device :: HwDevice -- ^ Which hardware wallet brand the profile is dedicated to.
  } deriving (Show,Eq,Ord)

instance ToJSON Profile where
  toJSON Profile{..} =
    object [ "account_index" .= _accountIndex 
           , "alias" .= _alias
           , "device" .= _device
           ]

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \o ->
    Profile 
      <$> o .: "account_index"
      <*> o .: "alias"
      <*> o .: "device"

instance Default Profile where
  def = Profile
    { _accountIndex = 0
    , _alias = ""
    , _device = Ledger
    }
