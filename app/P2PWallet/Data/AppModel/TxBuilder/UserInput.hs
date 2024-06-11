{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilder.UserInput where

import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Data.Wallets.PaymentWallet
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- User Inputs
-------------------------------------------------
-- | Information for a user input. These are non-defi spending inputs.
data UserInput = UserInput
  { utxoRef :: TxOutRef
  -- | The bech32 address for this input. This is used to get any required key hashes.
  , paymentAddress :: PaymentAddress
  -- | The path to the required hw key for witnessing.
  , paymentKeyPath :: Maybe DerivationPath 
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | Whether the widget expands the info for this input.
  , showDetails :: Bool 
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''UserInput

fromPersonalUTxO :: Text -> PaymentAddress -> Maybe DerivationPath -> PersonalUTxO -> UserInput
fromPersonalUTxO alias paymentAddress mKeyPath PersonalUTxO{utxoRef,lovelace,nativeAssets} = UserInput
  { utxoRef = utxoRef
  , paymentAddress = paymentAddress
  , paymentKeyPath = mKeyPath
  , lovelace = lovelace
  , nativeAssets = nativeAssets
  , showDetails = False
  , walletAlias = alias
  }
