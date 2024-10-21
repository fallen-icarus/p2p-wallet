{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotBidAcceptance where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Spot Bid Acceptance
-------------------------------------------------
-- | Information for accepting a claim bid.
data SpotBidAcceptance = SpotBidAcceptance
  -- | The bid being accepted.
  { bidUTxO :: AftermarketUTxO
  -- | The market wallet for this seller
  , marketWallet :: MarketWallet
  -- | Which network the contracts are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SpotBidAcceptance

instance Default SpotBidAcceptance where
  def = SpotBidAcceptance
    { bidUTxO = def
    , network = def
    , marketWallet = def
    }

instance AssetBalancesForChange (a,SpotBidAcceptance) where
  assetBalancesForChange xs =
      ( sum $ for xs $ \(_, SpotBidAcceptance{..}) -> 
          -- The bid ada is free.
          maybe 0 (Lovelace . view #quantity) $ 
            aftermarketUTxOBuyerPrice bidUTxO >>= 
              find ((=="") . view #policyId) . map toNativeAsset . view #unPrices
      , sumNativeAssets $ concat $
          for xs $ \(_,SpotBidAcceptance{..}) ->
            let (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts bidUTxO
             in concat
                  -- The bid native assets are free.
                  [ filterOutBeacons $ bidUTxO ^. #nativeAssets
                  -- Need the nfts.
                  , map (set #quantity (-1) . mkNativeAsset policyId) names
                  ]
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter ((/=Aftermarket.beaconCurrencySymbol) . view #policyId)

aftermarketUTxOToSpotBidAcceptance 
  :: Network 
  -> MarketWallet
  -> AftermarketUTxO 
  -> SpotBidAcceptance
aftermarketUTxOToSpotBidAcceptance network marketWallet utxo = SpotBidAcceptance
  { bidUTxO = utxo
  , network = network
  , marketWallet = marketWallet
  }
