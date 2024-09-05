{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Lending scene is dedicated to Cardano-Loans `LoanWallet`.

-}
module P2PWallet.Data.AppModel.LendingModel 
  ( LendingScene(..)
  , LendingEvent(..)
  , LendingModel(..)
  , CachedBorrowerInfo
  , CachedLoanHistories

  , module P2PWallet.Data.AppModel.LendingModel.BorrowModel
  , module P2PWallet.Data.AppModel.LendingModel.LendModel
  , module P2PWallet.Data.AppModel.LendingModel.ResearchModel
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.LendingModel.BorrowModel
import P2PWallet.Data.AppModel.LendingModel.LendModel
import P2PWallet.Data.AppModel.LendingModel.ResearchModel
import P2PWallet.Data.Core.BorrowerInformation
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Loan Event History
-------------------------------------------------
-- | A type alias for a map from LoanId to loan event history as well as its current status.
type CachedLoanHistories = Map.Map Loans.LoanId ([LoanEvent],Maybe LoanUTxO)

-------------------------------------------------
-- Cached Borrower Information
-------------------------------------------------
-- | A type alias for a map from BorrowerId to BorrowerInformation.
type CachedBorrowerInfo = Map.Map Loans.BorrowerId BorrowerInformation

-------------------------------------------------
-- Lending Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Lending page.
data LendingScene
  -- | Information for the staking credential used as a borrower.
  = BorrowScene
  -- | Information for the staking credential used as a lender.
  | LendScene
  -- | Research open offers and active loans.
  | ResearchLoansScene
  deriving (Eq,Show)

-------------------------------------------------
-- Loans Page Events
-------------------------------------------------
-- | The possible UI events on the Loans page.
data LendingEvent
  -- | Change the Loans scene to the specified scene.
  = ChangeLendingScene LendingScene
  -- | Add a new loan wallet using one of the known staking credentials.
  | AddNewLoanWallet (AddEvent LoanWallet LoanWallet)
  -- | Delete a loan wallet.
  | DeleteLoanWallet (DeleteWithConfirmationEvent LoanWallet)
  -- | Open the more popup widget
  | ShowLendingMorePopup
  -- | An event for the BorrowModel.
  | BorrowEvent BorrowEvent
  -- | An event for the LendModel.
  | LendEvent LendEvent
  -- | An event for the LoanResearchModel.
  | LoanResearchEvent LoanResearchEvent
  -- | Lookup a specific loan's event history.
  | LookupLoanHistory (ProcessEvent Loans.LoanId CachedLoanHistories)
  -- | Lookup a specific borrower's information.
  | LookupBorrowerInformation (ProcessEvent (Loans.BorrowerId,PaymentAddress) CachedBorrowerInfo)

-------------------------------------------------
-- Lending State
-------------------------------------------------
data LendingModel = LendingModel
  -- | The current subscene.
  { scene :: LendingScene
  -- | The currently focused `LoanWallet` from the list of tracked `LoanWallet`s.
  , selectedWallet :: LoanWallet
  -- | The stake wallet to possibly use for the new loan wallet. This enables previewing
  -- the stake wallet's info before confirming.
  , targetStakeCredential :: Maybe StakeWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The borrow model.
  , borrowModel :: BorrowModel
  -- | The lend model.
  , lendModel :: LendModel
  -- | The research model.
  , researchModel :: LoanResearchModel
  -- | The cached loan histories.
  , cachedLoanHistories :: CachedLoanHistories
  -- | The cached borrower information.
  , cachedBorrowerInfo :: CachedBorrowerInfo
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LendingModel

instance Default LendingModel where
  def = LendingModel
    { scene = BorrowScene
    , selectedWallet = def
    , targetStakeCredential = Nothing
    , addingWallet = False
    , deletingWallet = False
    , showMorePopup = False
    , borrowModel = def
    , lendModel = def
    , researchModel = def
    , cachedLoanHistories = mempty
    , cachedBorrowerInfo = mempty
    }
