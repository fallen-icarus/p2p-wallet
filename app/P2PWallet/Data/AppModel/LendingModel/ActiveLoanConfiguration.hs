{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.ActiveLoanConfiguration where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Configuration
-------------------------------------------------
-- | The kind of offer the user is looking for.
data ActiveLoanConfiguration = ActiveLoanConfiguration
  { loanAsset :: Maybe NativeAsset
  , collateral :: [NativeAsset]
  , minDuration :: Maybe Integer
  , maxDuration :: Maybe Integer
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''ActiveLoanConfiguration

instance Default ActiveLoanConfiguration where
  def = ActiveLoanConfiguration
    { collateral = []
    , loanAsset = Nothing
    , minDuration = Nothing
    , maxDuration = Nothing
    }

-- | The user input for the kind of offer the user is looking for.
data NewActiveLoanConfiguration = NewActiveLoanConfiguration
  { loanAsset :: Text
  -- | The collateral asset names separated by newlines.
  , collateral :: Text
  , minDuration :: Text
  , maxDuration :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewActiveLoanConfiguration

instance Default NewActiveLoanConfiguration where
  def = NewActiveLoanConfiguration
    { loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    }

verifyNewActiveLoanConfiguration 
  :: TickerMap 
  -> NewActiveLoanConfiguration 
  -> Either Text ActiveLoanConfiguration
verifyNewActiveLoanConfiguration tickerMap NewActiveLoanConfiguration{..} = do
  verifiedLoanAsset <- case loanAsset of
    "" -> return Nothing
    xs -> Just <$> parseNativeAssetName tickerMap xs

  verifiedCollateral <- mapM (parseNativeAssetName tickerMap) $ lines collateral

  let durationParseErrorMsg xs = unlines
        [ "Could not parse: " <> xs
        , "It must be a positive whole number of days."
        ]
  verifiedMinDuration <- case minDuration of
    "" -> return Nothing
    xs -> fmap Just $ maybeToRight (durationParseErrorMsg xs) $ readMaybe $ toString xs

  verifiedMaxDuration <- case maxDuration of
    "" -> return Nothing
    xs -> fmap Just $ maybeToRight (durationParseErrorMsg xs) $ readMaybe $ toString xs

  whenJust verifiedMinDuration $ \duration ->
    when (duration < 0) $
      Left "Minimum duration must be positive."

  whenJust verifiedMaxDuration $ \duration ->
    when (duration < 0) $
      Left "Maximum duration must be positive."

  return $ ActiveLoanConfiguration
    { loanAsset = verifiedLoanAsset
    , minDuration = verifiedMinDuration
    , maxDuration = verifiedMaxDuration
    , collateral = verifiedCollateral
    }

toNewActiveLoanConfiguration :: ReverseTickerMap -> ActiveLoanConfiguration -> NewActiveLoanConfiguration
toNewActiveLoanConfiguration reverseTickerMap ActiveLoanConfiguration{..} = NewActiveLoanConfiguration
  { loanAsset = maybe "" (showAssetNameOnly reverseTickerMap) loanAsset
  , minDuration = maybe "" show minDuration
  , maxDuration = maybe "" show maxDuration
  , collateral = mconcat $ intersperse "\n" $ map (showAssetNameOnly reverseTickerMap) collateral
  }
