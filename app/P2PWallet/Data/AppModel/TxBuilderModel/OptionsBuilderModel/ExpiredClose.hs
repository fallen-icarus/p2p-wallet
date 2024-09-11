{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ExpiredClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Expired Options Close
-------------------------------------------------
-- | Information for closing an expired active options contract.
data ExpiredOptionsClose = ExpiredOptionsClose
  { utxoRef :: TxOutRef
  -- | The stake credential for this writer.
  , writerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current terms.
  , activeDatum :: Maybe Options.ActiveDatum
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Which network this is for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ExpiredOptionsClose

instance AssetBalancesForChange (a,ExpiredOptionsClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= activeBeaconCurrencySymbol

optionsUTxOToExpiredOptionsClose
  :: Network
  -> Text 
  -> Credential
  -> Maybe DerivationInfo
  -> OptionsUTxO 
  -> ExpiredOptionsClose
optionsUTxOToExpiredOptionsClose network alias stakeCredential mKeyInfo utxo = 
  ExpiredOptionsClose
    { utxoRef = utxo ^. #utxoRef
    , writerCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , lovelace = utxo ^. #lovelace
    , nativeAssets = utxo ^. #nativeAssets
    , activeDatum = optionsUTxOActiveDatum utxo
    , walletAlias = alias
    , network = network
    }
