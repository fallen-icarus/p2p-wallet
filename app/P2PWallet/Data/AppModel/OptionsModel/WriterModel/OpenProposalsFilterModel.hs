{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.OptionsModel.WriterModel.OpenProposalsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Open Proposals Filter Model
-------------------------------------------------
-- | Possible sortings.
data OpenProposalsSortMethod
  -- | By utxo output reference.
  = OpenProposalsLexicographically
  -- | By the quantity of the offer asset. 
  | OpenProposalsOfferAmount
  -- | By the requested premium.
  | OpenProposalsPremium
  -- | By the strike price.
  | OpenProposalsStrikePrice
  -- | By the duration of the contract. This goes by the min/max expiration inside the possible
  -- terms.
  | OpenProposalsExpiration
  -- | By the time the proposal was last "touched".
  | OpenProposalsTime
  deriving (Show,Eq,Enum)

instance Display OpenProposalsSortMethod where
  display OpenProposalsLexicographically = "Lexicographically"
  display OpenProposalsOfferAmount = "Offer Amount"
  display OpenProposalsPremium = "Premium"
  display OpenProposalsStrikePrice = "Strike Price"
  display OpenProposalsExpiration = "Expiration"
  display OpenProposalsTime = "Chronologically"

data OpenProposalsFilterModel = OpenProposalsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The proposal must use the specified offer asset.
  , offerAsset :: Text
  -- | The proposal must use the specified ask asset.
  , askAsset :: Text
  -- | The proposal must use the specified premium asset.
  , premiumAsset :: Text
  -- | Whether the proposal should be expired.
  , shouldBeExpired :: Maybe Bool
  -- | The current sorting method for the open Proposals.
  , sortingMethod :: OpenProposalsSortMethod
  -- | The current sorting direction for the open Proposals.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OpenProposalsFilterModel

instance Default OpenProposalsFilterModel where
  def = OpenProposalsFilterModel
    { scene = FilterScene
    , offerAsset = ""
    , askAsset = ""
    , premiumAsset = ""
    , shouldBeExpired = Nothing
    , sortingMethod = OpenProposalsTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkOpenProposalsFilterModel :: TickerMap -> OpenProposalsFilterModel -> Either Text ()
checkOpenProposalsFilterModel tickerMap OpenProposalsFilterModel{..} = do
  unless ("" == offerAsset) $
    void $ parseNativeAssetName tickerMap offerAsset

  unless ("" == askAsset) $
    void $ parseNativeAssetName tickerMap askAsset

  unless ("" == premiumAsset) $
    void $ parseNativeAssetName tickerMap premiumAsset
