{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Options scene is dedicated to Cardano-Options `OptionsWallet`.

-}
module P2PWallet.Data.AppModel.OptionsModel 
  ( OptionsScene(..)
  , OptionsEvent(..)
  , OptionsModel(..)

  , CachedOptionsProposals

  , module P2PWallet.Data.AppModel.OptionsModel.BuyerModel
  , module P2PWallet.Data.AppModel.OptionsModel.WriterModel
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.OptionsModel.BuyerModel
import P2PWallet.Data.AppModel.OptionsModel.WriterModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Options Proposals
-------------------------------------------------
-- | A type alias for a map from trading pair to known proposals.
type CachedOptionsProposals = Map.Map (OfferAsset, AskAsset, Maybe PremiumAsset) [OptionsUTxO]

-------------------------------------------------
-- Options Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Options page.
data OptionsScene
  -- | Information for the staking credential used as an options' writer.
  = OptionsWriterScene
  -- | Information for the staking credential used as a lender.
  | OptionsBuyerScene
  -- | Research options.
  | ResearchOptionsScene
  deriving (Eq,Show)

-------------------------------------------------
-- Options Page Events
-------------------------------------------------
-- | The possible UI events on the Options page.
data OptionsEvent
  -- | Change the Options scene to the specified scene.
  = ChangeOptionsScene OptionsScene
  -- | Add a new options wallet using one of the known staking credentials.
  | AddNewOptionsWallet (AddEvent OptionsWallet OptionsWallet)
  -- | Delete an options wallet.
  | DeleteOptionsWallet (DeleteWithConfirmationEvent OptionsWallet)
  -- | Open the more popup widget
  | ShowOptionsMorePopup
  -- | Sell Options Event.
  | OptionsWriterEvent OptionsWriterEvent
  -- | Buy Options Event.
  | OptionsBuyerEvent OptionsBuyerEvent
  -- | Sync the proposals for the selected contract assets.
  | SyncOptionsProposals (ProcessEvent (OfferAsset, AskAsset, Maybe PremiumAsset) CachedOptionsProposals)

-------------------------------------------------
-- Options State
-------------------------------------------------
data OptionsModel = OptionsModel
  -- | The current subscene.
  { scene :: OptionsScene
  -- | The currently focused `OptionsWallet` from the list of tracked `OptionsWallet`s.
  , selectedWallet :: OptionsWallet
  -- | The stake wallet to possibly use for the new options' wallet. This enables previewing
  -- the stake wallet's info before confirming.
  , targetStakeCredential :: Maybe StakeWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The sell model.
  , writerModel :: OptionsWriterModel
  -- | The buy model.
  , buyerModel :: OptionsBuyerModel
  -- | Cached proposal contracts.
  , cachedProposals :: CachedOptionsProposals
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsModel

instance Default OptionsModel where
  def = OptionsModel
    { scene = OptionsWriterScene
    , selectedWallet = def
    , targetStakeCredential = Nothing
    , addingWallet = False
    , deletingWallet = False
    , showMorePopup = False
    , writerModel = def
    , buyerModel = def
    , cachedProposals = mempty
    }
