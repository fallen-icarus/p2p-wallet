{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.OptionsModel.BuyerModel.AllProposalsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Prelude

-------------------------------------------------
-- All Proposals Filter Model
-------------------------------------------------
-- | Possible sortings.
data AllProposalsSortMethod
  -- | By the quantity of the offer asset. 
  = AllProposalsOfferAmount
  -- | By the requested premium.
  | AllProposalsPremium
  -- | By the strike price.
  | AllProposalsStrikePrice
  -- | By the duration of the contract. This goes by the min/max expiration inside the possible
  -- terms.
  | AllProposalsExpiration
  deriving (Show,Eq,Enum)

instance Display AllProposalsSortMethod where
  display AllProposalsOfferAmount = "Offer Amount"
  display AllProposalsPremium = "Premium"
  display AllProposalsStrikePrice = "Strike Price"
  display AllProposalsExpiration = "Expiration"

data AllProposalsFilterModel = AllProposalsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The current sorting method for the proposals.
  , sortingMethod :: AllProposalsSortMethod
  -- | The current sorting direction for the proposals.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AllProposalsFilterModel

instance Default AllProposalsFilterModel where
  def = AllProposalsFilterModel
    { scene = SortScene
    , sortingMethod = AllProposalsStrikePrice
    , sortingDirection = SortAscending
    }

