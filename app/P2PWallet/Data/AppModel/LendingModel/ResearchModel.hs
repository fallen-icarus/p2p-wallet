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

  , OfferResearchSortMethod(..)
  , OfferResearchFilterModel(..)

  , LoanResearchScene(..)
  , LoanResearchEvent(..)
  , LoanResearchModel(..)

  , module P2PWallet.Data.AppModel.LendingModel.LoanOfferConfiguration
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
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
lookupCachedOffers offerCfg cache = Map.lookup correctedCfg cache
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
insertIntoCachedOffers offerCfg result cache = Map.insert correctedCfg result cache
  where
    correctedCfg = offerCfg 
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
-- | The possible UI events on the Lend Scene
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
  -- | Whether to show the filter widget for offers.
  , showOffersFilter :: Bool
  -- | The offers filter model.
  , offersFilterModel :: OfferResearchFilterModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanResearchModel

instance Default LoanResearchModel where
  def = LoanResearchModel
    { scene = ResearchLoanOffers
    , newLoanOfferConfiguration = Nothing
    , selectedLoanOfferConfiguration = Nothing
    , inspectedBorrower = Nothing
    , inspectedLoan = Nothing
    , cachedLoanOffers = mempty
    , showOffersFilter = False
    , offersFilterModel = def
    }
