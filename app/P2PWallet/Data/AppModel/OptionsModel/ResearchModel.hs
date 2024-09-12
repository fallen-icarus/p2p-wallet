{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Research scene is dedicated to looking up what other options contracts have for their terms.

-}
module P2PWallet.Data.AppModel.OptionsModel.ResearchModel
  ( OptionsResearchEvent(..)
  , OptionsResearchModel(..)

  , module P2PWallet.Data.AppModel.OptionsModel.ResearchModel.AllActivesFilterModel
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.OptionsModel.ResearchModel.AllActivesFilterModel
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Research Scene Events
-------------------------------------------------
-- | The possible UI events on the Research Scene
data OptionsResearchEvent
  -- | Set the new trading pair as the selected trading pair for proposals
  = SetResearchActiveContractAssets (AddEvent (Text,Text) (OfferAsset, AskAsset))
  -- | Reset the filters for the active contracts.
  | ResetResearchAllActivesFilters

-------------------------------------------------
-- Research Model
-------------------------------------------------
data OptionsResearchModel = OptionsResearchModel
  -- | The selected trading pair for active contracts.
  { selectedActiveContractAssets :: Maybe (OfferAsset, AskAsset)
  -- | Whether the active contract trading pair widget should be open.
  , choosingActiveContractAssets :: Bool
  -- | The new active contract trading pair information.
  , newActiveContractAssets :: (Text,Text)
  -- | Show the active filter widget.
  , showActiveFilter :: Bool
  -- | The actives filter model.
  , activesFilterModel :: AllActiveOptionsFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsResearchModel

instance Default OptionsResearchModel where
  def = OptionsResearchModel
    { selectedActiveContractAssets = Nothing
    , choosingActiveContractAssets = False
    , newActiveContractAssets = ("","")
    , showActiveFilter = False
    , activesFilterModel = def
    }
