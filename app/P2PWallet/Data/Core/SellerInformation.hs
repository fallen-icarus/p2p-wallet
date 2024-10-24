{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.SellerInformation where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Prelude

-------------------------------------------------
-- Seller Information
-------------------------------------------------
-- | The seller information is the information a prospective buyer sees when deciding what to bid.
data SellerInformation = SellerInformation
  { sellerAddress :: PaymentAddress
  , currentSales :: [AftermarketUTxO]
  , showCurrentSales :: Bool
  , currentBids :: [AftermarketUTxO]
  , showCurrentBids :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SellerInformation

instance Default SellerInformation where
  def = SellerInformation
    { sellerAddress = ""
    , currentSales = []
    , showCurrentSales = False
    , currentBids = []
    , showCurrentBids = False
    }
