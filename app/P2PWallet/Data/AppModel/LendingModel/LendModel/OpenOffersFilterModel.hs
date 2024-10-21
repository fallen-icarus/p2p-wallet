{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.LendModel.OpenOffersFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Open Offers Filter Model
-------------------------------------------------
-- | Possible sortings.
data OpenOffersSortMethod
  -- | By utxo output reference.
  = OpenOffersLexicographically
  -- | By the quantity of the loan asset offered. This is sort lexicographically by name first if
  -- there are asks for different loan assets.
  | OpenOffersLoanAmount
  -- | By the duration of the loan.
  | OpenOffersDuration
  -- | By the time the offer was last "touched".
  | OpenOffersTime
  -- | By the interest rate for the loan.
  | OpenOffersInterest
  -- | By the expiration for the offer.
  | OpenOffersExpiration
  deriving (Show,Eq,Enum)

instance Display OpenOffersSortMethod where
  display OpenOffersLexicographically = "Lexicographically"
  display OpenOffersLoanAmount = "Loan Amount"
  display OpenOffersDuration = "Duration"
  display OpenOffersTime = "Chronologically"
  display OpenOffersInterest = "Interest Rate"
  display OpenOffersExpiration = "Offer Expiration"

data OpenOffersFilterModel = OpenOffersFilterModel
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
  -- | Whether the offer should be expired.
  , shouldBeExpired :: Maybe Bool
  -- | The current sorting method for the open offers.
  , sortingMethod :: OpenOffersSortMethod
  -- | The current sorting direction for the open offers.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OpenOffersFilterModel

instance Default OpenOffersFilterModel where
  def = OpenOffersFilterModel
    { scene = FilterScene
    , loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    , shouldBeExpired = Nothing
    , sortingMethod = OpenOffersTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkOpenOffersFilterModel :: TickerMap -> OpenOffersFilterModel -> Either Text ()
checkOpenOffersFilterModel tickerMap OpenOffersFilterModel{..} = do
  unless ("" == loanAsset) $
    void $ parseNativeAssetName tickerMap loanAsset

  mapM_ (parseNativeAssetName tickerMap) (lines collateral)

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
