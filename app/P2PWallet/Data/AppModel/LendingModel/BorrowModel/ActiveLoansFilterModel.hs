{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.BorrowModel.ActiveLoansFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Active Loans Filter Model
-------------------------------------------------
-- | Possible sortings.
data ActiveLoansSortMethod
  -- | By utxo output reference.
  = ActiveLoansLexicographically
  -- | By the quantity of the loan asset still owed.
  | ActiveLoansBalance
  -- | By the time the offer was last "touched".
  | ActiveLoansTime
  -- | By the interest rate for the loan.
  | ActiveLoansInterest
  -- | By the next deadline.
  | ActiveLoansDeadline
  -- | By the next required payment due before the next deadline
  | ActiveLoansRequiredPayment
  deriving (Show,Eq,Enum)

instance Display ActiveLoansSortMethod where
  display ActiveLoansLexicographically = "Lexicographically"
  display ActiveLoansBalance = "Balance Owed"
  display ActiveLoansTime = "Chronologically"
  display ActiveLoansInterest = "Interest Rate"
  display ActiveLoansDeadline = "Next Deadline"
  display ActiveLoansRequiredPayment = "Required Payment"

data ActiveLoansFilterModel = ActiveLoansFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The loan must offer the specified loan asset.
  , loanAsset :: Text
  -- | The loan must include these collateral assets. They are separated by newlines.
  , collateral :: Text
  -- | The minimum duration of the loan.
  , minDuration :: Text
  -- | The maximum duration of the loan.
  , maxDuration :: Text
  -- | Whether to filter by expired status.
  , loanExpired :: Maybe Bool
  -- | Whether to filter by claim expired status.
  , claimExpired :: Maybe Bool
  -- | The current sorting method for the active loans.
  , sortingMethod :: ActiveLoansSortMethod
  -- | The current sorting direction for the active loans.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ActiveLoansFilterModel

instance Default ActiveLoansFilterModel where
  def = ActiveLoansFilterModel
    { scene = FilterScene
    , loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    , loanExpired = Nothing
    , claimExpired = Nothing
    , sortingMethod = ActiveLoansDeadline
    , sortingDirection = SortAscending
    }

-- | Verify the information is valid.
checkActiveLoansFilterModel :: TickerMap -> ActiveLoansFilterModel -> Either Text ()
checkActiveLoansFilterModel tickerMap ActiveLoansFilterModel{..} = do
  unless ("" == loanAsset) $
    void $ parseNativeAssetName tickerMap loanAsset

  mapM_ (parseNativeAssetName tickerMap) $ lines collateral

  let durationParseErrorMsg xs = unlines
        [ "Could not parse: " <> xs
        , "It must be a positive whole number of days."
        ]
  verifiedMinDuration <- case minDuration of
    "" -> return Nothing
    xs -> fmap Just $ maybeToRight (durationParseErrorMsg xs) $ readMaybe @Integer $ toString xs

  verifiedMaxDuration <- case maxDuration of
    "" -> return Nothing
    xs -> fmap Just $ maybeToRight (durationParseErrorMsg xs) $ readMaybe @Integer $ toString xs

  whenJust verifiedMinDuration $ \duration ->
    when (duration < 0) $
      Left "Minimum duration must be positive."

  whenJust verifiedMaxDuration $ \duration ->
    when (duration < 0) $
      Left "Maximum duration must be positive."
