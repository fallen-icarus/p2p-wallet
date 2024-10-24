{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleUpdate where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleClose
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Sale Update
-------------------------------------------------
-- | A sale update is just the composition of closing one sale and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data SaleUpdate = SaleUpdate
  { oldSale :: SaleClose
  , newSale :: SaleCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SaleUpdate

instance AssetBalancesForChange (a,SaleUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldSale) xs
    , assetBalancesForChange $ map (over _2 $ view #newSale) xs
    ]

-- | Create a populated `NewSaleCreation` based on the current sale terms.
aftermarketUTxOToNewSaleCreation 
  :: ReverseTickerMap 
  -> MarketWallet
  -- | Either the first wallet in the tracked payment wallet list or the
  -- payment wallet already associated with the sale.
  -> PaymentWallet 
  -> AftermarketUTxO
  -> NewSaleCreation
aftermarketUTxOToNewSaleCreation reverseTickerMap marketWallet paymentWallet u = 
    NewSaleCreation
      { marketWallet = marketWallet
      , isAuction = isJust $ u ^? #marketDatum % _Just % _AuctionDatum
      , paymentWallet = paymentWallet
      , nfts = nfts
      , salePrice = mconcat 
                  $ intersperse "\n" 
                  $ map (showAssetBalance True reverseTickerMap) prices
      , network = marketWallet ^. #network
      , showNfts = False
      }
  where
    (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts u
    nfts = map (\name -> mkNativeAsset policyId name & #quantity .~ 1) names
    prices = maybe [] (map toNativeAsset . Aftermarket.unPrices) 
           $ aftermarketUTxOSellerPrice u
