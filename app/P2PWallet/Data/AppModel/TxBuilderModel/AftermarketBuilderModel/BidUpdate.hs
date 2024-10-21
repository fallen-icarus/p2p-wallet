{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUpdate where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidClose
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidCreation
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Bid Update
-------------------------------------------------
-- | A bid update is just the composition of closing one bid and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data BidUpdate = BidUpdate
  { oldBid :: BidClose
  , newBid :: BidCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BidUpdate

instance AssetBalancesForChange (a,BidUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldBid) xs
    , assetBalancesForChange $ map (over _2 $ view #newBid) xs
    ]
