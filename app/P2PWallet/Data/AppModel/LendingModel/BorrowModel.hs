{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Borrow scene is dedicated to the `LoanWallet` being used to borrow.

-}
module P2PWallet.Data.AppModel.LendingModel.BorrowModel 
  ( BorrowScene(..)
  , BorrowEvent(..)
  , BorrowModel(..)

  , module P2PWallet.Data.AppModel.LendingModel.BorrowModel.LenderOffersFilterModel
  , module P2PWallet.Data.AppModel.LendingModel.BorrowModel.OpenAsksFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.LendingModel.BorrowModel.LenderOffersFilterModel
import P2PWallet.Data.AppModel.LendingModel.BorrowModel.OpenAsksFilterModel
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data BorrowScene
  -- | All current asks that belong to this borrower.
  = OpenAsks
  -- | All current offers to this borrower.
  | CurrentOffers
  -- | All active loans for this borrower.
  | ActiveLoans
  -- | This borrower's credit history.
  | CreditHistory
  -- | The transaction history for the loan address.
  | BorrowerTransactions
  deriving (Eq,Show)

-------------------------------------------------
-- Borrow Scene Events
-------------------------------------------------
-- | The possible UI events on the Borrow Scene
data BorrowEvent
  -- | Change the Borrow subscene to the specified subscene.
  = ChangeBorrowScene BorrowScene
  -- | Create a new Ask UTxO.
  | CreateNewAsk (AddEvent NewAskCreation AskCreation)
  -- | Add the new ask close to the transaction builder.
  | AddSelectedAskClose LoanUTxO
  -- | Add the new ask update to the transaction builder.
  | AddSelectedAskUpdate (AddEvent LoanUTxO AskUpdate)
  -- | Verify the open asks filter model has valid information.
  | CheckOpenAsksFilterModel
  -- | Reset the open asks fitler model.
  | ResetOpenAsksFilters
  -- | Verify the lender offers filter model has valid information.
  | CheckLenderOffersFilterModel
  -- | Reset the lender offers fitler model.
  | ResetLenderOffersFilters

-------------------------------------------------
-- Borrow State
-------------------------------------------------
-- | The state for the borrow scene.
data BorrowModel = BorrowModel
  -- | The current borrow subscene.
  { scene :: BorrowScene
  -- | The new ask to create.
  , newAskCreation :: Maybe NewAskCreation
  -- | The new ask update.
  , newAskUpdate :: Maybe (LoanUTxO,NewAskCreation)
  -- | Whether to show the filter widget for open asks.
  , showOpenAsksFilter :: Bool
  -- | The open asks filter model.
  , openAsksFilterModel :: OpenAsksFilterModel
  -- | Whether to show the filter widget for lender offers.
  , showLenderOffersFilter :: Bool
  -- | The lender offers filter model.
  , lenderOffersFilterModel :: LenderOffersFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BorrowModel

instance Default BorrowModel where
  def = BorrowModel
    { scene = OpenAsks
    , newAskCreation = Nothing
    , newAskUpdate = Nothing
    , showOpenAsksFilter = False
    , openAsksFilterModel = def
    , showLenderOffersFilter = False
    , lenderOffersFilterModel = def
    }