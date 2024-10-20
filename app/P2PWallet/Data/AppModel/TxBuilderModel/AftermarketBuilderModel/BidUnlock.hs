{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUnlock where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Bid Unlock
-------------------------------------------------
-- | Information for closing an accepted bid that's claim period has passed.
data BidUnlock = BidUnlock
  -- | The bid UTxO to close.
  { bidUTxO :: AftermarketUTxO
  -- | The market wallet for the seller.
  , sellerWallet :: MarketWallet
  -- | Which network this is for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BidUnlock

instance AssetBalancesForChange (a,BidUnlock) where
  assetBalancesForChange xs =
      ( sum $ map (view (#bidUTxO % #lovelace) . snd) xs
      , filterOutBeacons $ concatMap (view (#bidUTxO % #nativeAssets) . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter ((/= Aftermarket.beaconCurrencySymbol) . view #policyId)

aftermarketUTxOToBidUnlock :: Network -> MarketWallet -> AftermarketUTxO -> BidUnlock
aftermarketUTxOToBidUnlock network marketWallet bidUTxO = 
  BidUnlock
    { bidUTxO = bidUTxO
    , sellerWallet = marketWallet
    , network = network
    }
