{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.LoanAskConfiguration where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Ask Configuration
-------------------------------------------------
-- | The kind of ask the Lender is looking for.
data LoanAskConfiguration = LoanAskConfiguration
  { loanAsset :: Maybe NativeAsset
  , collateral :: [NativeAsset]
  , minDuration :: Maybe Integer
  , maxDuration :: Maybe Integer
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''LoanAskConfiguration

instance Default LoanAskConfiguration where
  def = LoanAskConfiguration
    { loanAsset = Nothing
    , collateral = []
    , minDuration = Nothing
    , maxDuration = Nothing
    }

-- | The user input for the kind of ask the Lender is looking for.
data NewLoanAskConfiguration = NewLoanAskConfiguration
  { loanAsset :: Text
  -- | The collateral asset names separated by newlines.
  , collateral :: Text
  , minDuration :: Text
  , maxDuration :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewLoanAskConfiguration

instance Default NewLoanAskConfiguration where
  def = NewLoanAskConfiguration
    { loanAsset = ""
    , collateral = ""
    , minDuration = ""
    , maxDuration = ""
    }

verifyNewLoanAskConfiguration 
  :: TickerMap 
  -> NewLoanAskConfiguration 
  -> Either Text LoanAskConfiguration
verifyNewLoanAskConfiguration tickerMap NewLoanAskConfiguration{..} = do
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

  return $ LoanAskConfiguration
    { loanAsset = verifiedLoanAsset
    , collateral = verifiedCollateral
    , minDuration = verifiedMinDuration
    , maxDuration = verifiedMaxDuration
    }

toNewLoanAskConfiguration :: ReverseTickerMap -> LoanAskConfiguration -> NewLoanAskConfiguration
toNewLoanAskConfiguration reverseTickerMap LoanAskConfiguration{..} = NewLoanAskConfiguration
  { loanAsset = maybe "" (showAssetNameOnly reverseTickerMap) loanAsset
  , collateral = mconcat $ intersperse "\n" $ map (showAssetNameOnly reverseTickerMap) collateral
  , minDuration = maybe "" show minDuration
  , maxDuration = maybe "" show maxDuration
  }
