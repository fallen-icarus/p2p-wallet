{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.CollateralInput where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.PaymentWallet
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Collateral Input
-------------------------------------------------
-- | Information for a collateral input.
data CollateralInput = CollateralInput
  -- | The collateral input's output reference.
  { utxoRef :: TxOutRef
  -- | The bech32 address for this input. This is used to get any required key hashes.
  , paymentAddress :: PaymentAddress
  -- | The path to the required hw key for witnessing.
  , paymentKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | The required collateral percentage of the fee.
  , collateralPercentage :: Decimal
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''CollateralInput

instance Default CollateralInput where
  def = CollateralInput
    { utxoRef = TxOutRef "" 0
    , paymentAddress = ""
    , paymentKeyDerivation = Nothing
    , lovelace = 0
    , nativeAssets = []
    , walletAlias = ""
    , collateralPercentage = 0
    }

instance AddToTxBody CollateralInput where
  addToTxBody txBody CollateralInput{..} = 
      txBody 
        -- Add the collateral input.
        & #collateralInput ?~ newCollateral
        -- Add the witness if required. Duplicate witnesses are removed by the `Semigroup` 
        -- instance of `TxBody`. They will also be sorted.
        & #keyWitnesses %~ maybe id (:) requiredWitness
    where 
      newCollateral :: TxBodyCollateral
      newCollateral = TxBodyCollateral
        { utxoRef = utxoRef
        , lovelace = lovelace
        , paymentAddress = paymentAddress
        , requiredCollateral = calculateRequiredCollateral (txBody ^. #fee) collateralPercentage
        }

      plutusAddress :: PlutusAddress
      plutusAddress =
        -- This should have been verified at an earlier step.
        fromRight (Address (PubKeyCredential "") Nothing) $ paymentAddressToPlutusAddress paymentAddress

      requiredWitness :: Maybe KeyWitness
      requiredWitness = case toPubKeyHash plutusAddress of
          Nothing -> Nothing
          Just pkHash -> Just $ KeyWitness (walletAlias, pkHash, paymentKeyDerivation)

-- | Multiply the transaction fee by the collateralPercentage. Add one lovelace to
-- account for potential rounding issues.
calculateRequiredCollateral :: Lovelace -> Decimal -> Lovelace
calculateRequiredCollateral fee collateralPercentage
  | fee == 0 = fee
  | otherwise = (+1) $ toLovelace $ Ada $ unAda (toAda fee) * collateralPercentage

personalUTxOToCollateralInput
  :: Text 
  -> PaymentAddress 
  -> Maybe DerivationInfo
  -> PersonalUTxO 
  -> CollateralInput
personalUTxOToCollateralInput alias paymentAddress mKeyInfo PersonalUTxO{utxoRef,lovelace,nativeAssets} = 
  CollateralInput
    { utxoRef = utxoRef
    , paymentAddress = paymentAddress
    , paymentKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , walletAlias = alias
    , collateralPercentage = 0
    }
