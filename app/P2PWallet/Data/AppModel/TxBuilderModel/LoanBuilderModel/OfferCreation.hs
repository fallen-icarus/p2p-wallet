{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation where

import Data.Text qualified as T

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Creation
-------------------------------------------------
-- | Information for a new offer.
data OfferCreation = OfferCreation
  -- | The target bech32 address for the borrower.
  { loanAddress :: PaymentAddress
  -- | The credential for the target borrower.
  , borrowerCredential :: Credential
  -- | The credential for the lender.
  , lenderCredential :: Credential
  -- | The borrower credential's required hw key path for witnessing.
  , lenderKeyDerivation :: Maybe DerivationInfo
  -- | The address the key nft must go to.
  , paymentWallet :: PaymentWallet
  -- | A counter-offer for the loan's duration. The default is to offer the same duration as the
  -- target ask.
  , loanTerm :: Integer
  -- | A counter-offer for the loan's principal. The default is to offer the same loan asset and
  -- amount as the target ask.
  , loanAmount :: NativeAsset
  -- | The interest that must be periodically applied, specified as a percentage.
  , interest :: Rational
  -- | The frequency at which interest must be applied, specified as number of days.
  , compoundFrequency :: Maybe Integer
  -- | The required minimum payment due each compound period.
  , minPayment :: Integer
  -- | The penalty to apply if the minimum payment is not met before the next compounding.
  , penalty :: Loans.Penalty
  -- | The asset collateralization.
  , collateralization :: [(NativeAsset,Rational)]
  -- | Whether the collateral can be swapped out during the loan.
  , collateralIsSwappable :: Bool
  -- | The number of days desired to claim expired collateral.
  , claimPeriod :: Integer
  -- | The number of days until the offer expires.
  , offerExpiration :: Maybe Integer
  -- | The amount of ada used for the min UTxO value. 
  , deposit :: Lovelace
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the lender credential.
  , alias :: Text
  -- | Current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OfferCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,OfferCreation) where
  assetBalancesForChange xs =
    ( sum $ for xs $ \(_,OfferCreation{loanAmount,deposit}) -> 
        -- The total amount of ada is the deposit + the loan principal if ada is the loan
        -- asset.
        negate $ case loanAmount ^. #policyId of
          "" -> deposit + Lovelace (loanAmount ^. #quantity)
          _ -> deposit
    , flip concatMap xs $ \(_,OfferCreation{loanAmount}) -> 
        -- Native assets are only required for the loan amount itself.
        case loanAmount ^. #policyId of
          "" -> []
          _ -> [loanAmount & #quantity %~ negate]
    )

-------------------------------------------------
-- New Offer Creation
-------------------------------------------------
-- | Information from the user that will be verified and converted to an `OfferCreation`.
data NewOfferCreation = NewOfferCreation
  -- | The target bech32 address for the borrower.
  { loanAddress :: PaymentAddress
  -- | The credential for the target borrower.
  , borrowerCredential :: Credential
  -- | The credential for the lender.
  , lenderCredential :: Credential
  -- | The borrower credential's required hw key path for witnessing.
  , lenderKeyDerivation :: Maybe DerivationInfo
  -- | The address the key NFT must go to. This will be a dropdown menu for the currently track
  -- payment wallets.
  , paymentWallet :: PaymentWallet
  -- | A counter-offer for the loan's duration. The default is to offer the same duration as the
  -- target ask.
  , loanTerm :: Integer
  -- | A counter-offer for the loan's principal. The default is to offer the same loan asset and
  -- amount as the target ask.
  , loanAmount :: Text
  -- | The interest that must be periodically applied, specified as a percentage. Can be left blank
  -- for an interest free loan.
  , interest :: Text
  -- | The frequency at which interest must be applied, specified as number of days.
  , compoundFrequency :: Text
  -- | The required minimum payment due each compound period. This can only be set when 
  -- compoundFrequency is not "".
  , minPayment :: Text
  -- | The penalty to apply if the minimum payment is not met before the next compounding.
  -- This can only be set when minPayment is > 0.
  , penalty :: (NewPenalty,Text)
  -- | The asset collateralization. They are assumed to be of the format: 'name, price' and
  -- separated by newlines.
  , collateralization :: Text
  -- | Whether the collateral can be swapped out during the loan.
  , collateralIsSwappable :: Bool
  -- | The number of days desired to claim expired collateral.
  , claimPeriod :: Integer
  -- | The number of days until the offer expires.
  , offerExpiration :: Text 
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the lender credential.
  , alias :: Text
  } deriving (Show,Eq)

data NewPenalty
  = NoNewPenalty
  | NewFixedPenalty
  | NewPercentPenalty
  deriving (Show,Eq,Enum)

instance Display NewPenalty where
  display NoNewPenalty = "No Penalty"
  display NewFixedPenalty = "Fixed Fee Penalty"
  display NewPercentPenalty = "Percent Penalty"

fromPenalty :: ReverseTickerMap -> NativeAsset -> Loans.Penalty -> (NewPenalty, Text)
fromPenalty reverseTickerMap loanAsset penalty = case penalty of
  Loans.NoPenalty -> (NoNewPenalty, "")
  Loans.FixedFee fee -> 
    (NewFixedPenalty, showAssetQuantityOnly reverseTickerMap $ loanAsset & #quantity .~ fee)
  Loans.PercentFee percent -> (NewPercentPenalty, displayPercentage $ toRational percent)

makeFieldLabelsNoPrefix ''NewOfferCreation

instance Default NewOfferCreation where
  def = NewOfferCreation
    { loanAddress = ""
    , borrowerCredential = PubKeyCredential ""
    , lenderCredential = PubKeyCredential ""
    , lenderKeyDerivation = Nothing
    , paymentWallet = def
    , loanAmount = ""
    , loanTerm = 30
    , interest = "5"
    , compoundFrequency = ""
    , minPayment = "0"
    , penalty = (NoNewPenalty, "0")
    , collateralization = ""
    , collateralIsSwappable = False
    , claimPeriod = 30
    , offerExpiration = ""
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewOfferCreation`.
createNewOfferCreation 
  :: ReverseTickerMap
  -> LoanUTxO 
  -> LoanWallet
  -> PaymentWallet
  -> NewOfferCreation
createNewOfferCreation reverseTickerMap u@LoanUTxO{loanAddress} lenderWallet paymentWallet =
    def & #network .~ lenderWallet ^. #network
        & #loanAddress .~ loanAddress
        & #borrowerCredential .~ 
            -- The beacons guarantee the address has a staking credential.
            fromRight (PubKeyCredential "") (paymentAddressStakeCredential loanAddress)
        & #lenderCredential .~ lenderWallet ^. #stakeCredential
        & #alias .~ lenderWallet ^. #alias
        & #lenderKeyDerivation .~ lenderWallet ^. #stakeKeyDerivation
        & #collateralization .~ showCollateralization collateralization
        & #paymentWallet .~ paymentWallet
        & #loanTerm .~ calcDaysInPosixPeriod (fromPlutusTime $ askDatum ^. #loanTerm)
        & #loanAmount .~ showAssetBalance True reverseTickerMap askLoanAmount
  where 
    askDatum :: Loans.AskDatum
    askDatum = fromMaybe def $ loanUTxOAskDatum u

    askLoanAmount :: NativeAsset
    askLoanAmount = 
      toNativeAsset (askDatum ^. #loanAsset) & #quantity .~ askDatum ^. #loanPrincipal

    collateralization :: [(NativeAsset,Rational)]
    collateralization = zip (map toNativeAsset $ askDatum ^. #collateral % #unCollateral) $ repeat 0

    showCollateralization = mconcat . intersperse "\n" . map displayCollateralization

    displayCollateralization :: (NativeAsset,Rational) -> Text
    displayCollateralization (asset, price) = 
      showAssetNameOnly reverseTickerMap asset <> ", " <> 
        showPriceFormatted reverseTickerMap asset askLoanAmount price

-------------------------------------------------
-- NewOfferCreation <--> OfferCreation
-------------------------------------------------
-- | Verify the user info for the new offer.
verifyNewOfferCreation 
  :: ReverseTickerMap 
  -> TickerMap 
  -> POSIXTime -- ^ Current time.
  -> NewOfferCreation 
  -> Either Text OfferCreation
verifyNewOfferCreation reverseTickerMap tickerMap currentTime NewOfferCreation{..} = do
    -- Check that the loan asset is valid. No fingerprints can be used.
    verifiedLoanAmount <-
      first (const $ parseErrorMsg loanAmount) $ parseNativeAssets tickerMap mempty loanAmount

    -- Check the interest.
    verifiedInterest <- case interest of
      "" -> return 0
      xs -> maybeToRight ("Could not parse interest: " <> xs) $ parsePercentage xs

    when (verifiedInterest < 0) $
      Left "Interest must be > 0"

    -- Check the compoundFrequency.
    verifiedCompoundFrequency <- case compoundFrequency of
      "" -> return Nothing
      xs -> fmap Just $ maybeToRight ("Could not parse compound frequency: " <> xs) $ 
        readMaybe @Integer $ toString xs

    whenJust verifiedCompoundFrequency $ \freq ->
      when (freq <= 0) $
        Left "Compound frequency must be > 0 if set. Leave blank to not set a frequency."

    -- Check the minPayment.
    verifiedMinPayment <- case (verifiedCompoundFrequency, minPayment) of
      (Nothing, _) -> return 0 -- minPayment can only be used with a compound period.
      (_, "") -> return 0
      (_, xs) -> fmap (view #quantity) $
        parseFormattedAssetQuantity reverseTickerMap verifiedLoanAmount xs

    when (verifiedMinPayment < 0) $
      Left "Minimum payment must be > 0"

    -- Check the penalty.
    verifiedPenalty <- case (verifiedMinPayment > 0, penalty) of
      -- penalties can only be set when minimum payments are required
      (False, _) -> return Loans.NoPenalty
      (_, (NoNewPenalty, _)) -> return Loans.NoPenalty
      (_, (NewFixedPenalty, xs)) -> 
        fmap (Loans.FixedFee . view #quantity) $
          parseFormattedAssetQuantity reverseTickerMap verifiedLoanAmount xs
      (_, (NewPercentPenalty, xs)) -> 
        fmap (Loans.PercentFee . fromRational) $ 
          maybeToRight ("Could not parse percent penalty: " <> xs) $ parsePercentage xs

    -- Check the collateralization.
    verifiedCollateralization <- mapM (parseCollateralization verifiedLoanAmount) $ 
      lines collateralization

    when (length (ordNubOn fst verifiedCollateralization) /= length verifiedCollateralization) $
      Left "No duplicate assets in collateralization list."

    -- Check the claim period.
    when (claimPeriod <= 0) $ Left "Number of days for the claim period must be greater than 0."

    -- Check the offer expiration.
    verifiedExpiration <- case offerExpiration of
      "" -> return Nothing
      xs -> fmap Just $ maybeToRight ("Could not parse days until offer expiration: " <> xs) $ 
        readMaybe @Integer $ toString xs

    -- Check that the expiration is valid.
    whenJust verifiedExpiration $ \days ->
      when (days <= 0) $ Left "Number of days for the expiration must be greater than 0."

    -- Check that the loan term is valid.
    when (loanTerm <= 0) $ Left "Number of days for the duration must be greater than 0."

    return $ OfferCreation
      { loanAddress = loanAddress
      , borrowerCredential = borrowerCredential
      , lenderCredential = lenderCredential
      , lenderKeyDerivation = lenderKeyDerivation
      , paymentWallet = paymentWallet
      , loanAmount = verifiedLoanAmount
      , loanTerm = loanTerm
      , interest = verifiedInterest
      , compoundFrequency = verifiedCompoundFrequency
      , minPayment = verifiedMinPayment
      , penalty = verifiedPenalty
      , collateralization = verifiedCollateralization
      , collateralIsSwappable = collateralIsSwappable
      , claimPeriod = claimPeriod
      , offerExpiration = verifiedExpiration
      , deposit = 
          -- This is just a placeholder since the minUTxOValue depends on the size of the
          -- datum. This will be replaced by the actual value calculated for the minUTxOValue.
          9_999_999
      , network = network
      , alias = alias
      , currentTime = toPlutusTime currentTime
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

    parseCollateralization :: NativeAsset -> Text -> Either Text (NativeAsset,Rational)
    parseCollateralization loanAsset t = case words $ T.replace "," " " t of
      [name, price] -> do
        verifiedName <- parseNativeAssetName tickerMap name
        verifiedPrice <- parseFormattedPrice reverseTickerMap verifiedName loanAsset price
        return (verifiedName, verifiedPrice)
      _ -> Left $ "Could not parse: " <> t

toNewOfferCreation :: ReverseTickerMap -> OfferCreation -> NewOfferCreation
toNewOfferCreation reverseTickerMap OfferCreation{..} = NewOfferCreation
    { loanAddress = loanAddress
    , borrowerCredential = borrowerCredential
    , lenderCredential = lenderCredential
    , lenderKeyDerivation = lenderKeyDerivation
    , paymentWallet = paymentWallet
    , loanTerm = loanTerm
    , loanAmount = showAssetBalance True reverseTickerMap loanAmount
    , interest = displayPercentage interest
    , compoundFrequency = maybe "" show compoundFrequency
    , minPayment = showAssetQuantityOnly reverseTickerMap $ loanAmount & #quantity .~ minPayment
    , penalty = fromPenalty reverseTickerMap loanAmount penalty
    , collateralization = 
        mconcat $ intersperse "\n" $ map displayCollateralization collateralization
    , collateralIsSwappable = collateralIsSwappable
    , claimPeriod = claimPeriod
    , offerExpiration = maybe "" show offerExpiration
    , network = network
    , alias = alias
    }
  where
    displayCollateralization :: (NativeAsset,Rational) -> Text
    displayCollateralization (asset, price) = 
      showAssetNameOnly reverseTickerMap asset <> ", " <> 
        showPriceFormatted reverseTickerMap asset loanAmount price
