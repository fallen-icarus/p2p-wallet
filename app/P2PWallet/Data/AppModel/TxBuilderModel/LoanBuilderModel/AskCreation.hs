{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Ask Creation
-------------------------------------------------
-- | Information for a new ask.
data AskCreation = AskCreation
  -- | The target bech32 address for the new ask.
  { paymentAddress :: PaymentAddress
  -- | The credential for the borrower.
  , borrowerCredential :: Credential
  -- | The borrower credential's required hw key path for witnessing.
  , borrowerKeyDerivation :: Maybe DerivationInfo
  -- | The size of the loan.
  , loanAmount :: NativeAsset
  -- | How many days the loan will be active for. The total time will be 24 hrs * number of days.
  , loanTerm :: Integer
  -- | The assets that will be used as collateral
  , collateral :: [NativeAsset]
  -- | The number of desired new asks with these details.
  , count :: Int
  -- | The amount of ada used for the min UTxO value. 
  , deposit :: Lovelace
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the borrower credential.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AskCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,AskCreation) where
  assetBalancesForChange xs =
      -- Increase the quantity of lovelace for each output by the count. 
      ( sum $ for xs $ \(_,AskCreation{count,deposit}) -> fromIntegral (negate count) * deposit
      -- Increase the quantity of each native asset by the count.
      , flip concatMap xs $ \(_,AskCreation{count,collateral}) -> 
          mapMaybe (toNativeAssetQuantity (negate count)) collateral
      )
    where
      toNativeAssetQuantity :: Int -> NativeAsset -> Maybe NativeAsset
      toNativeAssetQuantity count asset@NativeAsset{policyId}
        -- Ada is not a native asset and is already part of the deposit.
        | policyId == "" = Nothing
        -- One unit of each asset must be deposited in each output.
        | otherwise = Just $ asset & #quantity .~ fromIntegral count

-------------------------------------------------
-- New Ask Creation
-------------------------------------------------
-- | Information from the user that will be verified and converted to an `AskCreation`.
data NewAskCreation = NewAskCreation
  -- | The target bech32 address for the new ask.
  { paymentAddress :: PaymentAddress
  -- | The credential for the borrower.
  , borrowerCredential :: Credential
  -- | The borrower credential's required hw key path for witnessing.
  , borrowerKeyDerivation :: Maybe DerivationInfo
  -- | The size of the loan specified as '# name' where the name is either a ticker or on-chain
  -- name.
  , loanAmount :: Text
  -- | How many days the loan will be active for. The total time will be 24 hrs * number of days.
  , loanTerm :: Integer
  -- | The assets that will be used as collateral. They are assumed to be separated by newlines.
  , collateral :: Text
  -- | This is used internally to preserve the current count when converting back from AskCreation.
  , count :: Int
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the borrower credential.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewAskCreation

instance Default NewAskCreation where
  def = NewAskCreation
    { paymentAddress = ""
    , borrowerCredential = PubKeyCredential ""
    , borrowerKeyDerivation = Nothing
    , loanAmount = ""
    , loanTerm = 30
    , collateral = ""
    , count = 1
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewAskCreation`.
createNewAskCreation 
  :: Network 
  -> Credential 
  -> Maybe DerivationInfo 
  -> PaymentAddress 
  -> Text 
  -> NewAskCreation
createNewAskCreation network borrowerCredential derivationInfo paymentAddress alias =
  def & #network .~ network
      & #paymentAddress .~ paymentAddress
      & #borrowerCredential .~ borrowerCredential
      & #alias .~ alias
      & #borrowerKeyDerivation .~ derivationInfo

-------------------------------------------------
-- NewAskCreation <--> AskCreation
-------------------------------------------------
-- | Verify the user info for the new ask.
verifyNewAskCreation :: TickerMap -> NewAskCreation -> Either Text AskCreation
verifyNewAskCreation tickerMap NewAskCreation{..} = do
  -- Check that the loan asset is valid. No fingerprints can be used.
  verifiedLoanAmount <- case lines loanAmount of
    [x] -> first (const $ parseErrorMsg x) $ parseNativeAssets tickerMap mempty x
    _ -> Left "Invalid loan amount."

  -- Check that the assets are valid. Returns the first error, if any. No fingerprints can be used.
  verifiedCollateral <- mapM (parseNativeAssetName tickerMap) $ lines collateral

  when (null verifiedCollateral) $ Left "At least one asset must be specified for collateral."

  -- Check that the loan term is valid.
  when (loanTerm <= 0) $ Left "Number of days must be greater than 0."

  -- Create the new output.
  return $ AskCreation
    { paymentAddress = paymentAddress
    , borrowerCredential = borrowerCredential
    , loanAmount = verifiedLoanAmount
    , loanTerm = loanTerm
    , collateral = verifiedCollateral
    , count = count
    , deposit = 0 -- This will be set later.
    , network = network
    , alias = alias
    , borrowerKeyDerivation = borrowerKeyDerivation
    }

  where
    -- A custom error message is used since fingerprints are not allowed.
    parseErrorMsg :: Text -> Text
    parseErrorMsg x = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# ticker'"
      , ""
      , "Could not parse: '" <> x <> "'"
      , ""
      , "If using a ticker, make sure it is in the Ticker Registry."
      ]

toNewAskCreation :: ReverseTickerMap -> AskCreation -> NewAskCreation
toNewAskCreation reverseTickerMap AskCreation{..} = NewAskCreation
  { paymentAddress = paymentAddress
  , borrowerCredential = borrowerCredential
  , loanAmount = showAssetBalance True reverseTickerMap loanAmount
  , loanTerm = loanTerm
  , collateral = mconcat $ intersperse "\n" $ map (showAssetNameOnly reverseTickerMap) collateral
  , count = count
  , network = network
  , alias = alias
  , borrowerKeyDerivation = borrowerKeyDerivation
  }
