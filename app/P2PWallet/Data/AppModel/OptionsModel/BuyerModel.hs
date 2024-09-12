{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Buy scene is dedicated to buying options proposal UTxOs.

-}
module P2PWallet.Data.AppModel.OptionsModel.BuyerModel
  ( OptionsBuyerEvent(..)
  , OptionsBuyerModel(..)
  
  , module P2PWallet.Data.AppModel.OptionsModel.BuyerModel.AllProposalsFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.OptionsModel.BuyerModel.AllProposalsFilterModel
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Buy Scene Events
-------------------------------------------------
-- | The possible UI events on the Buy Scene
data OptionsBuyerEvent
  -- | Set the new trading pair as the selected trading pair.
  = SetNewContractAssets (AddEvent (Text,Text,Text) (OfferAsset, AskAsset, Maybe PremiumAsset))
  -- | Purchase Options proposal.
  | PurchaseOptionsProposal (ProcessEvent (Integer,OptionsUTxO) ProposalPurchase)
  -- | Reset the proposals fitler model.
  | ResetAllProposalsFilters

-------------------------------------------------
-- Buy State
-------------------------------------------------
-- | The state for the Buy scene.
data OptionsBuyerModel = OptionsBuyerModel
  -- | The selected trading pair. The premium asset is optional.
  { selectedContractAssets :: Maybe (OfferAsset, AskAsset, Maybe PremiumAsset)
  -- | Whether the trading pair widget should be open.
  , choosingContractAssets :: Bool
  -- | The new trading pair information.
  , newContractAssets :: (Text,Text,Text)
  -- | Whether to show the proposal filter widget.
  , showProposalFilter :: Bool
  -- | The proposals filter model.
  , proposalsFilterModel :: AllProposalsFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsBuyerModel

instance Default OptionsBuyerModel where
  def = OptionsBuyerModel
    { selectedContractAssets = Nothing
    , choosingContractAssets = False
    , newContractAssets = ("","","")
    , showProposalFilter = False
    , proposalsFilterModel = def
    }
