{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Lend scene is dedicated to the `LoanWallet` being used to lend.

-}
module P2PWallet.Data.AppModel.LendingModel.LendModel 
  ( CachedLoanAsks 
  , LendScene(..)
  , LendEvent(..)
  , LendModel(..)

  , module P2PWallet.Data.AppModel.LendingModel.LendModel.OpenOffersFilterModel
  , module P2PWallet.Data.AppModel.LendingModel.LendModel.RequestsFilterModel
  , module P2PWallet.Data.AppModel.LendingModel.LoanAskConfiguration
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.LendingModel.LendModel.OpenOffersFilterModel
import P2PWallet.Data.AppModel.LendingModel.LendModel.RequestsFilterModel
import P2PWallet.Data.AppModel.LendingModel.LoanAskConfiguration
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Asks
-------------------------------------------------
-- | A type alias for a map from `LoanAskConfiguration` to Ask UTxOs.
type CachedLoanAsks = Map.Map LoanAskConfiguration [LoanUTxO]

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data LendScene
  -- | All current offers that belong to this lender.
  = OpenOffers
  -- | All transactions involving this lender id.
  | OfferHistory
  -- | Browse all current requests for loans.
  | ViewLoanRequests
  deriving (Eq,Show)

-------------------------------------------------
-- Lend Scene Events
-------------------------------------------------
-- | The possible UI events on the Lend Scene
data LendEvent
  -- | Change the Lend subscene to the specified subscene.
  = ChangeLendScene LendScene
  -- | Set the new loan ask configuration when none has been set before.
  | InitializeLoanAskConfiguration (AddEvent NewLoanAskConfiguration LoanAskConfiguration)
  -- | Sync the loan asks for the selected ask configuration.
  | SyncLoanAsks (ProcessEvent CachedLoanAsks)
  -- | Update the loan ask configuration that is already set.
  | UpdateLoanAskConfiguration (ProcessEvent LoanAskConfiguration)
  -- | Create a new Offer UTxO.
  | CreateNewOffer (AddEvent LoanUTxO OfferCreation)
  -- | Add the new offer close to the transaction builder.
  | AddSelectedOfferClose LoanUTxO
  -- | Add the new offer update to the transaction builder.
  | AddSelectedOfferUpdate (AddEvent LoanUTxO OfferUpdate)
  -- | Verify the open offers filter model has valid information.
  | CheckOpenOffersFilterModel
  -- | Reset the open offers fitler model.
  | ResetOpenOffersFilters

-------------------------------------------------
-- Lend Model
-------------------------------------------------
data LendModel = LendModel
  -- | The current subscene.
  { scene :: LendScene
  -- | The new ask configuration to use.
  , newLoanAskConfiguration :: Maybe NewLoanAskConfiguration
  -- | The selected ask configuration to use.
  , selectedLoanAskConfiguration :: Maybe LoanAskConfiguration
  -- | Cached loan asks.
  , cachedLoanAsks :: CachedLoanAsks
  -- | Whether to show the filter widget for loan requests.
  , showViewRequestsFilter :: Bool
  -- | The requests filter model.
  , requestsFilterModel :: RequestsFilterModel
  -- | The new offer to create.
  , newOfferCreation :: Maybe NewOfferCreation
  -- | Whether to show the filter widget for open offers.
  , showOpenOffersFilter :: Bool
  -- | The open offers filter model.
  , openOffersFilterModel :: OpenOffersFilterModel
  -- | The new offer update.
  , newOfferUpdate :: Maybe (LoanUTxO,NewOfferCreation)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LendModel

instance Default LendModel where
  def = LendModel
    { scene = OpenOffers
    , newLoanAskConfiguration = Nothing
    , selectedLoanAskConfiguration = Nothing
    , cachedLoanAsks = mempty
    , showViewRequestsFilter = False
    , requestsFilterModel = def
    , newOfferCreation = def
    , showOpenOffersFilter = False
    , openOffersFilterModel = def
    , newOfferUpdate = Nothing
    }
