{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Research scene is dedicated to looking up what other loans have for their terms.

-}
module P2PWallet.Data.AppModel.LendingModel.ResearchModel
  ( CachedLoanOffers
  , lookupCachedOffers
  , insertIntoCachedOffers

  , CachedActiveLoans
  , lookupCachedActiveLoans
  , insertIntoCachedActiveLoans

  , OfferResearchSortMethod(..)
  , OfferResearchFilterModel(..)

  , ActiveResearchSortMethod(..)
  , ActiveResearchFilterModel(..)

  , LoanResearchScene(..)
  , LoanResearchEvent(..)
  , LoanResearchModel(..)

  , module P2PWallet.Data.AppModel.LendingModel.ActiveLoanConfiguration
  , module P2PWallet.Data.AppModel.LendingModel.LoanOfferConfiguration
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.LendingModel.ActiveLoanConfiguration
import P2PWallet.Data.AppModel.LendingModel.LoanOfferConfiguration
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Offers
-------------------------------------------------
-- | A type alias for a map from `LoanOfferConfiguration` to Offer UTxOs.
type CachedLoanOffers = Map.Map LoanOfferConfiguration [LoanUTxO]

-- | Not all of the fields in `LoanOfferConfiguration` are relevant for looking in the cache. This
-- function handles those fields.
lookupCachedOffers :: LoanOfferConfiguration -> CachedLoanOffers -> Maybe [LoanUTxO]
lookupCachedOffers offerCfg = Map.lookup correctedCfg
  where
    correctedCfg = offerCfg 
      & #collateral .~ []

-- | Not all of the fields in `LoanOfferConfiguration` are relevant for looking in the cache. This
-- function handles those fields.
insertIntoCachedOffers 
  :: LoanOfferConfiguration 
  -> [LoanUTxO] 
  -> CachedLoanOffers 
  -> CachedLoanOffers
insertIntoCachedOffers offerCfg = Map.insert correctedCfg
  where
    correctedCfg = offerCfg 
      & #collateral .~ []

-------------------------------------------------
-- Cached ActiveLoans
-------------------------------------------------
-- | A type alias for a map from `ActiveLoanConfiguration` to Active UTxOs.
type CachedActiveLoans = Map.Map ActiveLoanConfiguration [LoanUTxO]

-- | Not all of the fields in `ActiveLoanConfiguration` are relevant for looking in the cache. This
-- function handles those fields.
lookupCachedActiveLoans :: ActiveLoanConfiguration -> CachedActiveLoans -> Maybe [LoanUTxO]
lookupCachedActiveLoans activeCfg = Map.lookup correctedCfg
  where
    correctedCfg = activeCfg 
      & #collateral .~ []

-- | Not all of the fields in `ActiveLoanConfiguration` are relevant for looking in the cache. This
-- function handles those fields.
insertIntoCachedActiveLoans 
  :: ActiveLoanConfiguration
  -> [LoanUTxO] 
  -> CachedActiveLoans
  -> CachedActiveLoans
insertIntoCachedActiveLoans activeCfg = Map.insert correctedCfg
  where
    correctedCfg = activeCfg 
      & #collateral .~ []

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data LoanResearchScene
  -- | Research offers.
  = ResearchLoanOffers
  -- | Research active loans.
  | ResearchActiveLoans
  deriving (Eq,Show)

-------------------------------------------------
-- Research Scene Events
-------------------------------------------------
-- | The possible UI events on the Research Scene
data LoanResearchEvent
  -- | Change the Research subscene to the specified subscene.
  = ChangeLoanResearchScene LoanResearchScene
  -- | Set the new loan offer configuration when none has been set before.
  | InitializeLoanOfferConfiguration (AddEvent NewLoanOfferConfiguration LoanOfferConfiguration)
  -- | Sync the loan offers for the selected offer configuration.
  | SyncLoanOffers (ProcessEvent () CachedLoanOffers)
  -- | Update the loan offer configuration that is already set.
  | UpdateLoanOfferConfiguration (ProcessEvent () LoanOfferConfiguration)
  -- | Verify the offers filter model has valid information.
  | CheckResearchOffersFilterModel
  -- | Reset the offers fitler model.
  | ResetResearchOffersFilters
  -- | Lookup borrower information.
  | InspectResearchBorrowerInformation (Loans.BorrowerId, PaymentAddress)
  -- | Stop inspecting the borrower's information.
  | CloseInspectedResearchBorrowerInformation
  -- | Inspect Target Loan's history.
  | InspectResearchLoanHistory Loans.LoanId
  -- | Stop inspecting the loan's history.
  | CloseInspectedResearchLoanHistory
  -- | Set the new active loan configuration when none has been set before.
  | InitializeActiveLoanConfiguration (AddEvent NewActiveLoanConfiguration ActiveLoanConfiguration)
  -- | Sync the active loans for the selected active loan configuration.
  | SyncActiveLoans (ProcessEvent () CachedActiveLoans)
  -- | Update the active loans configuration that is already set.
  | UpdateActiveLoanConfiguration (ProcessEvent () ActiveLoanConfiguration)
  -- | Verify the active loans filter model has valid information.
  | CheckResearchActiveFilterModel
  -- | Reset the active loans fitler model.
  | ResetResearchActivesFilters

-------------------------------------------------
-- Offer Filter Model
-------------------------------------------------
-- | Possible sortings.
data OfferResearchSortMethod
  -- | By utxo output reference.
  = OfferResearchLexicographically
  -- | By the quantity of the loan asset offered. This is sort lexicographically by name first if
  -- there are offers for different loan assets.
  | OfferResearchLoanAmount
  -- | By the duration of the loan.
  | OfferResearchDuration
  -- | By the time the offer was last "touched".
  | OfferResearchTime
  -- | By the interest rate for the loan.
  | OfferResearchInterest
  deriving (Show,Eq,Enum)

instance Display OfferResearchSortMethod where
  display OfferResearchLexicographically = "Lexicographically"
  display OfferResearchLoanAmount = "Loan Amount"
  display OfferResearchDuration = "Duration"
  display OfferResearchTime = "Chronologically"
  display OfferResearchInterest = "Interest Rate"

data OfferResearchFilterModel = OfferResearchFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The target loan offer configuration to query.
  , newLoanOfferConfiguration :: NewLoanOfferConfiguration
  -- | The current sorting method for the offers.
  , sortingMethod :: OfferResearchSortMethod
  -- | The current sorting direction for the offers.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OfferResearchFilterModel

instance Default OfferResearchFilterModel where
  def = OfferResearchFilterModel
    { scene = FilterScene
    , newLoanOfferConfiguration = def
    , sortingMethod = OfferResearchTime
    , sortingDirection = SortDescending
    }

-------------------------------------------------
-- Active Filter Model
-------------------------------------------------
-- | Possible sortings.
data ActiveResearchSortMethod
  -- | By utxo output reference.
  = ActiveResearchLexicographically
  -- | By the quantity of the loan asset offered. This is sort lexicographically by name first if
  -- there are loans for different loan assets.
  | ActiveResearchLoanAmount
  -- | By the duration of the loan.
  | ActiveResearchDuration
  -- | By the time the offer was last "touched".
  | ActiveResearchTime
  -- | By the interest rate for the loan.
  | ActiveResearchInterest
  deriving (Show,Eq,Enum)

instance Display ActiveResearchSortMethod where
  display ActiveResearchLexicographically = "Lexicographically"
  display ActiveResearchLoanAmount = "Loan Amount"
  display ActiveResearchDuration = "Duration"
  display ActiveResearchTime = "Chronologically"
  display ActiveResearchInterest = "Interest Rate"

data ActiveResearchFilterModel = ActiveResearchFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The target loan offer configuration to query.
  , newActiveLoanConfiguration :: NewActiveLoanConfiguration
  -- | The current sorting method for the active loans.
  , sortingMethod :: ActiveResearchSortMethod
  -- | The current sorting direction for the active loans.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ActiveResearchFilterModel

instance Default ActiveResearchFilterModel where
  def = ActiveResearchFilterModel
    { scene = FilterScene
    , newActiveLoanConfiguration = def
    , sortingMethod = ActiveResearchTime
    , sortingDirection = SortDescending
    }

-------------------------------------------------
-- Research Model
-------------------------------------------------
data LoanResearchModel = LoanResearchModel
  -- | The current subscene.
  { scene :: LoanResearchScene
  -- | The new offer configuration to use.
  , newLoanOfferConfiguration :: Maybe NewLoanOfferConfiguration
  -- | The selected offer configuration to use.
  , selectedLoanOfferConfiguration :: Maybe LoanOfferConfiguration
  -- | Focused borrower.
  , inspectedBorrower :: Maybe (Loans.BorrowerId,PaymentAddress)
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | Cached loan offers.
  , cachedLoanOffers :: CachedLoanOffers
  -- | Cached active loans.
  , cachedActiveLoans :: CachedActiveLoans
  -- | The new active loan configuration to use.
  , newActiveLoanConfiguration :: Maybe NewActiveLoanConfiguration
  -- | The selected active loan configuration to use.
  , selectedActiveLoanConfiguration :: Maybe ActiveLoanConfiguration
  -- | Whether to show the filter widget for offers.
  , showOffersFilter :: Bool
  -- | The offers filter model.
  , offersFilterModel :: OfferResearchFilterModel
  -- | Whether to show the filter widget for active loans.
  , showActivesFilter :: Bool
  -- | The active loans filter model.
  , activesFilterModel :: ActiveResearchFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanResearchModel

instance Default LoanResearchModel where
  def = LoanResearchModel
    { scene = ResearchLoanOffers
    , newLoanOfferConfiguration = Nothing
    , selectedLoanOfferConfiguration = Nothing
    , newActiveLoanConfiguration = Nothing
    , selectedActiveLoanConfiguration = Nothing
    , inspectedBorrower = Nothing
    , inspectedLoan = Nothing
    , cachedLoanOffers = mempty
    , cachedActiveLoans = mempty
    , showOffersFilter = False
    , offersFilterModel = def
    , showActivesFilter = False
    , activesFilterModel = def
    }
