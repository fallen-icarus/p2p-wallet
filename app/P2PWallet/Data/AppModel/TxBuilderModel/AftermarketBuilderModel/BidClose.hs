{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Bid Close
-------------------------------------------------
-- | Information for closing a sale.
data BidClose = BidClose
  { utxoRef :: TxOutRef
  -- | The stake credential for this bidder.
  , bidderCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current bid terms.
  , marketDatum :: Maybe AftermarketDatum
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Which network this is for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BidClose

instance AssetBalancesForChange (a,BidClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= Aftermarket.beaconCurrencySymbol

aftermarketUTxOToBidClose 
  :: Network
  -> Text 
  -> Credential
  -> Maybe DerivationInfo
  -> AftermarketUTxO 
  -> BidClose
aftermarketUTxOToBidClose network alias stakeCredential mKeyInfo AftermarketUTxO{..} = 
  BidClose
    { utxoRef = utxoRef
    , bidderCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , marketDatum = marketDatum
    , walletAlias = alias
    , network = network
    }
