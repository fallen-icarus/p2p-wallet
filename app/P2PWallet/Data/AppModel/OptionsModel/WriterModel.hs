{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Sell scene is dedicated to the `OptionsWallet` being used to sell options contracts.

-}
module P2PWallet.Data.AppModel.OptionsModel.WriterModel
  ( OptionsWriterScene(..)
  , OptionsWriterEvent(..)
  , OptionsWriterModel(..)

  , module P2PWallet.Data.AppModel.OptionsModel.WriterModel.ActiveContractsFilterModel
  , module P2PWallet.Data.AppModel.OptionsModel.WriterModel.OpenProposalsFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.OptionsModel.WriterModel.ActiveContractsFilterModel
import P2PWallet.Data.AppModel.OptionsModel.WriterModel.OpenProposalsFilterModel
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data OptionsWriterScene
  -- | All current proposals that belong to this writer.
  = OpenOptionsProposals
  -- | All active contracts for this writer.
  | ActiveOptionsContracts
  -- | The transaction history for the options address.
  | OptionsTransactions
  deriving (Eq,Show)

-------------------------------------------------
-- Sell Scene Events
-------------------------------------------------
-- | The possible UI events on the Sell Scene
data OptionsWriterEvent
  -- | Change the Sell subscene to the specified subscene.
  = ChangeOptionsWriterScene OptionsWriterScene
  -- | Create a new Proposal UTxO.
  | CreateNewOptionsProposal (AddEvent NewProposalCreation ProposalCreation)
  -- | Add the new proposal close to the transaction builder.
  | AddSelectedProposalClose OptionsUTxO
  -- | Add the new proposal update to the transaction builder.
  | AddSelectedProposalUpdate (AddEvent OptionsUTxO ProposalUpdate)
  -- | Verify the open proposals filter model has valid information.
  | CheckOpenProposalsFilterModel
  -- | Reset the open proposals fitler model.
  | ResetOpenProposalsFilters
  -- | Add selected options close to transaction builder.
  | AddSelectedExpiredOptionsClose OptionsUTxO
  -- | Update options payment address.
  | UpdateWriterPaymentAddress (AddEvent OptionsUTxO WriterAddressUpdate)
  -- | Verify the active contracts filter model has valid information.
  | CheckActiveContractsFilterModel
  -- | Reset the active contracts fitler model.
  | ResetActiveContractsFilters

-------------------------------------------------
-- Sell State
-------------------------------------------------
-- | The state for the Sell scene.
data OptionsWriterModel = OptionsWriterModel
  -- | The current sell subscene.
  { scene :: OptionsWriterScene
  -- | The new proposal to create.
  , newProposalCreation :: Maybe NewProposalCreation
  -- | The new proposal update.
  , newProposalUpdate :: Maybe (OptionsUTxO,NewProposalCreation)
  -- | Whether to show the filter widget for proposals.
  , showProposalFilter :: Bool
  -- | The open proposals filter model.
  , proposalsFilterModel :: OpenProposalsFilterModel
  -- | The new writer address update.
  , newWriterAddressUpdate :: Maybe NewWriterAddressUpdate
  -- | Whether to show the filter widget for active contracts.
  , showActivesFilter :: Bool
  -- | The active contracts filter model.
  , activesFilterModel :: ActiveContractsFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsWriterModel

instance Default OptionsWriterModel where
  def = OptionsWriterModel
    { scene = OpenOptionsProposals
    , newProposalCreation = Nothing
    , showProposalFilter = False
    , newProposalUpdate = Nothing
    , proposalsFilterModel = def
    , newWriterAddressUpdate = Nothing
    , showActivesFilter = False
    , activesFilterModel = def
    }