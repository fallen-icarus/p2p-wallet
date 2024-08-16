{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.BorrowModel.LenderOffersFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Lender Offers Filter Model
-------------------------------------------------
-- | Possible sortings.
data LenderOffersSortMethod
  -- | By utxo output reference.
  = LenderOffersLexicographically
  -- | By the quantity of the loan asset requested. This is sort lexicographically by name first if
  -- there are asks for different loan assets.
  | LenderOffersLoanAmount
  -- | By the duration of the loan.
  | LenderOffersDuration
  -- | By the time the offer was last "touched".
  | LenderOffersTime
  -- | By the interest rate for the loan.
  | LenderOffersInterest
  deriving (Show,Eq,Enum)

instance Display LenderOffersSortMethod where
  display LenderOffersLexicographically = "Lexicographically"
  display LenderOffersLoanAmount = "Loan Amount"
  display LenderOffersDuration = "Duration"
  display LenderOffersTime = "Chronologically"
  display LenderOffersInterest = "Interest Rate"

data LenderOffersFilterModel = LenderOffersFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The offer must offer the specified loan asset.
  , loanAsset :: Text
  -- | The offer must include these collateral assets. They are separated by newlines.
  , collateral :: Text
  -- | The minimum duration of the loan.
  , minDuration :: Text
  -- | The maximum duration of the loan.
  , maxDuration :: Text
  -- | The current sorting method for the lender offers.
  , sortingMethod :: LenderOffersSortMethod
  -- | The current sorting direction for the lender offers.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LenderOffersFilterModel

instance Default LenderOffersFilterModel where
  def = LenderOffersFilterModel
    { scene = FilterScene
    , loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    , sortingMethod = LenderOffersInterest
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkLenderOffersFilterModel :: TickerMap -> LenderOffersFilterModel -> Either Text ()
checkLenderOffersFilterModel tickerMap LenderOffersFilterModel{..} = do
  case loanAsset of
    "" -> return ()
    xs -> const () <$> parseNativeAssetName tickerMap xs

  const () <$> (mapM (parseNativeAssetName tickerMap) $ lines collateral)

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

