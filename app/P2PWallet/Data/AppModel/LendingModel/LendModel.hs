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
  , LendTxFilterModel(..)

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
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
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
  | SyncLoanAsks (ProcessEvent () CachedLoanAsks)
  -- | Update the loan ask configuration that is already set.
  | UpdateLoanAskConfiguration (ProcessEvent () LoanAskConfiguration)
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
  -- | Lookup borrower information.
  | InspectProspectiveBorrowerInformation (Loans.BorrowerId, PaymentAddress)
  -- | Stop inspecting the borrower's information.
  | CloseInspectedProspectiveBorrowerInformation
  -- | Inspect Target Loan's history.
  | InspectTargetLoanHistory Loans.LoanId
  -- | Stop inspecting the loan's history.
  | CloseInspectedTargetLoanHistory
  -- | Reset lend tx filters.
  | ResetLendTxFilters
  -- | Inspect an offer Transaction.
  | InspectOfferTransaction Transaction
  -- | Stop inspecting the transaction.
  | CloseInspectedOfferTransaction

-------------------------------------------------
-- Transaction Filter Model
-------------------------------------------------
data LendTxFilterModel = LendTxFilterModel
  -- | The date range for displaying transactions.
  { dateRange :: (Maybe Day, Maybe Day)
  -- | The offer must offer the specified loan asset.
  , loanAsset :: Text
  -- | The offer must include these collateral assets. They are separated by newlines.
  , collateral :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LendTxFilterModel

instance Default LendTxFilterModel where
  def = LendTxFilterModel
    { dateRange = (Nothing,Nothing)
    , loanAsset = ""
    , collateral = ""
    }

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
  -- | Focused borrower.
  , inspectedBorrower :: Maybe (Loans.BorrowerId,PaymentAddress)
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | Whether to show the filter widget for transactions.
  , showTransactionFilter :: Bool
  -- | The transaction filter model.
  , txFilterModel :: LendTxFilterModel
  -- | The transaction filter model scene.
  , txFilterScene :: FilterScene
  -- | Focused offer transaction details.
  , inspectedOfferTransaction :: Maybe Transaction
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
    , inspectedBorrower = Nothing
    , inspectedLoan = Nothing
    , txFilterModel = def
    , txFilterScene = FilterScene
    , showTransactionFilter = False
    , inspectedOfferTransaction = Nothing
    }
