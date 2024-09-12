{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.OptionsModel.WriterModel.ActiveContractsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Active Contracts Filter Model
-------------------------------------------------
-- | Possible sortings.
data ActiveContractsSortMethod
  -- | By utxo output reference.
  = ActiveContractsLexicographically
  -- | By the quantity of the offer asset. 
  | ActiveContractsOfferAmount
  -- | By the quantity of the ask asset. 
  | ActiveContractsAskAmount
  -- | By the strike price.
  | ActiveContractsStrikePrice
  -- | By the expiration date.
  | ActiveContractsExpiration
  -- | By the time the contract utxo was last "touched".
  | ActiveContractsTime
  deriving (Show,Eq,Enum)

instance Display ActiveContractsSortMethod where
  display ActiveContractsLexicographically = "Lexicographically"
  display ActiveContractsOfferAmount = "Offer Amount"
  display ActiveContractsAskAmount = "Ask Amount"
  display ActiveContractsStrikePrice = "Strike Price"
  display ActiveContractsExpiration = "Expiration"
  display ActiveContractsTime = "Chronologically"

data ActiveContractsFilterModel = ActiveContractsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The contract must use the specified offer asset.
  , offerAsset :: Text
  -- | The contract must use the specified ask asset.
  , askAsset :: Text
  -- | Whether the contract should be expired.
  , shouldBeExpired :: Maybe Bool
  -- | The current sorting method for the active contracts.
  , sortingMethod :: ActiveContractsSortMethod
  -- | The current sorting direction for the active contracts.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ActiveContractsFilterModel

instance Default ActiveContractsFilterModel where
  def = ActiveContractsFilterModel
    { scene = FilterScene
    , offerAsset = ""
    , askAsset = ""
    , shouldBeExpired = Nothing
    , sortingMethod = ActiveContractsExpiration
    , sortingDirection = SortAscending
    }

-- | Verify the information is valid.
checkActiveContractsFilterModel :: TickerMap -> ActiveContractsFilterModel -> Either Text ()
checkActiveContractsFilterModel tickerMap ActiveContractsFilterModel{..} = do
  unless ("" == offerAsset) $
    void $ parseNativeAssetName tickerMap offerAsset

  unless ("" == askAsset) $
    void $ parseNativeAssetName tickerMap askAsset
