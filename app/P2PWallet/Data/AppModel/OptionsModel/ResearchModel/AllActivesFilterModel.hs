{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.OptionsModel.ResearchModel.AllActivesFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Prelude

-------------------------------------------------
-- Active Contracts Filter Model
-------------------------------------------------
-- | Possible sortings.
data AllActiveOptionsSortMethod
  -- | By utxo output reference.
  = AllActiveOptionsLexicographically
  -- | By the quantity of the offer asset. 
  | AllActiveOptionsOfferAmount
  -- | By the quantity of the ask asset. 
  | AllActiveOptionsAskAmount
  -- | By the strike price.
  | AllActiveOptionsStrikePrice
  -- | By the expiration date.
  | AllActiveOptionsExpiration
  -- | By the time the contract utxo was last "touched".
  | AllActiveOptionsTime
  deriving (Show,Eq,Enum)

instance Display AllActiveOptionsSortMethod where
  display AllActiveOptionsLexicographically = "Lexicographically"
  display AllActiveOptionsOfferAmount = "Offer Amount"
  display AllActiveOptionsAskAmount = "Ask Amount"
  display AllActiveOptionsStrikePrice = "Strike Price"
  display AllActiveOptionsExpiration = "Expiration"
  display AllActiveOptionsTime = "Chronologically"

data AllActiveOptionsFilterModel = AllActiveOptionsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The current sorting method for the active contracts.
  , sortingMethod :: AllActiveOptionsSortMethod
  -- | The current sorting direction for the active contracts.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AllActiveOptionsFilterModel

instance Default AllActiveOptionsFilterModel where
  def = AllActiveOptionsFilterModel
    { scene = SortScene
    , sortingMethod = AllActiveOptionsStrikePrice
    , sortingDirection = SortAscending
    }
