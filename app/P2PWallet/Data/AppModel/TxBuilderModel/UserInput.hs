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
  -- | The input's output reference.
  { utxoRef :: TxOutRef
  -- | The bech32 address for this input. This is used to get any required key hashes.
  , paymentAddress :: PaymentAddress
  -- | The path to the required hw key for witnessing.
  , paymentKeyDerivation :: Maybe DerivationInfo
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

instance AssetBalancesForChange (a,UserInput) where
  assetBalancesForChange xs =
    ( sum $ map (view #lovelace . snd) xs
    , concatMap (view #nativeAssets . snd) xs
    )

instance AddToTxBody UserInput where
  addToTxBody txBody UserInput{utxoRef,paymentAddress,paymentKeyDerivation} = 
      txBody 
        -- Add the input while preserving ordering.
        & #inputs %~ flip snoc newInput
        -- Add the witness if required. Duplicate witnesses are removed by the `Semigroup` 
        -- instance of `TxBody`. They will also be sorted.
        & #keyWitnesses %~ maybe id (:) requiredWitness
    where 
      newInput :: TxBodyInput
      newInput = TxBodyInput
        { utxoRef = utxoRef
        , spendingScriptInfo = Nothing
        }

      plutusAddress :: PlutusAddress
      plutusAddress =
        -- This should have been verified at an earlier step.
        fromRight (Address (PubKeyCredential "") Nothing) $ paymentAddressToPlutusAddress paymentAddress

      requiredWitness :: Maybe KeyWitness
      requiredWitness = case toPubKeyHash plutusAddress of
          Nothing -> Nothing
          Just pkHash -> Just $ KeyWitness (pkHash, paymentKeyDerivation)

personalUTxOToUserInput 
  :: Text 
  -> PaymentAddress 
  -> Maybe DerivationInfo
  -> PersonalUTxO 
  -> UserInput
personalUTxOToUserInput alias paymentAddress mKeyInfo PersonalUTxO{utxoRef,lovelace,nativeAssets} = 
  UserInput
    { utxoRef = utxoRef
    , paymentAddress = paymentAddress
    , paymentKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , showDetails = False
    , walletAlias = alias
    }
