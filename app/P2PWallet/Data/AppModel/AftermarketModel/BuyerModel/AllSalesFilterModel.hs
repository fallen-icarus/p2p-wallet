{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.AllSalesFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Prelude

-------------------------------------------------
-- All Sales Filter Model
-------------------------------------------------
-- | Possible sortings.
data AllSalesSortMethod
  -- | By utxo output reference.
  = AllSalesLexicographically
  -- | By the number nfts in the batch.
  | AllSalesNftCount
  -- | By the time the sale was last "touched".
  | AllSalesTime
  deriving (Show,Eq,Enum)

instance Display AllSalesSortMethod where
  display AllSalesLexicographically = "Lexicographically"
  display AllSalesNftCount = "Number of NFTs"
  display AllSalesTime = "Chronologically"

data AllSalesFilterModel = AllSalesFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | Whether the sale should be an auction. False for spot.
  , shouldBeAuction :: Maybe Bool
  -- | The current sorting method for the open Sales.
  , sortingMethod :: AllSalesSortMethod
  -- | The current sorting direction for the open Sales.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AllSalesFilterModel

instance Default AllSalesFilterModel where
  def = AllSalesFilterModel
    { scene = FilterScene
    , shouldBeAuction = Nothing
    , sortingMethod = AllSalesTime
    , sortingDirection = SortDescending
    }
