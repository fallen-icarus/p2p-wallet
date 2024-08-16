{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.BorrowModel.OpenAsksFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Open Asks Filter Model
-------------------------------------------------
-- | Possible sortings.
data OpenAsksSortMethod
  -- | By utxo output reference.
  = OpenAsksLexicographically
  -- | By the quantity of the loan asset requested. This is sort lexicographically by name first if
  -- there are asks for different loan assets.
  | OpenAsksLoanAmount
  -- | By the duration of the loan.
  | OpenAsksDuration
  -- | By the time the ask was last "touched".
  | OpenAsksTime
  deriving (Show,Eq,Enum)

instance Display OpenAsksSortMethod where
  display OpenAsksLexicographically = "Lexicographically"
  display OpenAsksLoanAmount = "Loan Amount"
  display OpenAsksDuration = "Duration"
  display OpenAsksTime = "Chronologically"

data OpenAsksFilterModel = OpenAsksFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The ask must request the specified loan asset.
  , loanAsset :: Text
  -- | The collateral asset names separated by newlines.
  , collateral :: Text
  -- | The minimum duration of the loan.
  , minDuration :: Text
  -- | The maximum duration of the loan.
  , maxDuration :: Text
  -- | The current sorting method for the open asks.
  , sortingMethod :: OpenAsksSortMethod
  -- | The current sorting direction for the open asks.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OpenAsksFilterModel

instance Default OpenAsksFilterModel where
  def = OpenAsksFilterModel
    { scene = FilterScene
    , loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    , sortingMethod = OpenAsksTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkOpenAsksFilterModel :: TickerMap -> OpenAsksFilterModel -> Either Text ()
checkOpenAsksFilterModel tickerMap OpenAsksFilterModel{..} = do
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

