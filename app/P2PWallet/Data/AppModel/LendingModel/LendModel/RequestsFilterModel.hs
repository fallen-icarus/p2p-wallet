{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.LendModel.RequestsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.LendingModel.LoanAskConfiguration
import P2PWallet.Prelude

-------------------------------------------------
-- Requests Filter Model
-------------------------------------------------
-- | Possible sortings.
data RequestsSortMethod
  -- | By utxo output reference.
  = RequestsLexicographically
  -- | By the quantity of the loan asset requested. This is sort lexicographically by name first if
  -- there are asks for different loan assets.
  | RequestsLoanAmount
  -- | By the duration of the loan.
  | RequestsDuration
  -- | By the time the ask was last "touched".
  | RequestsTime
  deriving (Show,Eq,Enum)

instance Display RequestsSortMethod where
  display RequestsLexicographically = "Lexicographically"
  display RequestsLoanAmount = "Loan Amount"
  display RequestsDuration = "Duration"
  display RequestsTime = "Chronologically"

data RequestsFilterModel = RequestsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The target loan ask configuration.
  , newLoanAskConfiguration :: NewLoanAskConfiguration
  -- | The current sorting method for the requests.
  , sortingMethod :: RequestsSortMethod
  -- | The current sorting direction for the requests.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''RequestsFilterModel

instance Default RequestsFilterModel where
  def = RequestsFilterModel
    { scene = FilterScene
    , newLoanAskConfiguration = def
    , sortingMethod = RequestsTime
    , sortingDirection = SortDescending
    }
