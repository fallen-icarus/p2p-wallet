{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.UserInput where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.PaymentWallet
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

instance AddToTxBody UserInput where
  addToTxBody txBody UserInput{utxoRef,paymentAddress,paymentKeyPath} = 
      txBody 
        -- Add the input while preserving ordering.
        & #inputs %~ flip snoc newInput
        -- Add the witness if required. Duplicate witnesses are removed by the `Semigroup` 
        -- instance of `TxBody`. They will also be sorted.
        & #witnesses %~ maybe id (:) requiredWitness
    where 
      newInput :: TxBodyInput
      newInput = TxBodyInput
        { utxoRef = utxoRef
        }

      plutusAddress :: PlutusAddress
      plutusAddress =
        -- This should have been verified at an earlier step.
        fromRight (Address (PubKeyCredential "") Nothing) $ paymentAddressToPlutusAddress paymentAddress

      requiredWitness :: Maybe Witness
      requiredWitness = case toPubKeyHash plutusAddress of
          Nothing -> Nothing
          Just pkHash -> Just $ Witness (pkHash, paymentKeyPath)

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
