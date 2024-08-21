{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanPayment where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Payment
-------------------------------------------------
-- | Information for a loan payment.
data LoanPayment = LoanPayment
  -- | The current loan.
  { activeUTxO :: LoanUTxO
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | Payment amount.
  , paymentAmount :: NativeAsset
  -- | The amount of each collateral asset being left.
  , collateralBalances :: [NativeAsset]
  -- | The calculated collateral deposit.
  , calculatedCollateralDeposit :: Lovelace
  -- | The extra minUTxOValue amount of ada required for the new active UTxO, relative to the
  -- input collateral UTxO.
  , collateralDeposit :: Lovelace
  -- | The minUTxOValue amount of ada required for the lender payment output.
  , paymentDeposit :: Lovelace
  -- | Which network the loans are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , alias :: Text
  -- | Whether the payment is a full payment.
  , isFullPayment :: Bool
  -- | The current time. This is used to determine the invalid-hereafter bound.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanPayment

instance AssetBalancesForChange (a,LoanPayment) where
  assetBalancesForChange xs =
      ( sum 
          [ sum $ map (view $ _2 % #activeUTxO % #lovelace) xs
          , sum $ map (negate . view (_2 % #calculatedCollateralDeposit)) xs
          , sum $ map (negate . view (_2 % #paymentDeposit)) xs
          , sum $ for xs $ \(_,payment) -> 
              if payment ^. #paymentAmount % #policyId == "" then
                Lovelace $ negate $ payment ^. #paymentAmount % #quantity
              else 0
          ]
      , filterOutBeacons $ sumNativeAssets $ mconcat
          [ concatMap (view $ _2 % #activeUTxO % #nativeAssets) xs
          , filter ((/= "") . view #policyId) $ map (over #quantity negate) $
              concatMap (view $ _2 % #collateralBalances) xs
          , filter ((/= "") . view #policyId) $
              for xs (over #quantity negate . view (_2 % #paymentAmount))
          ]
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= Loans.activeBeaconCurrencySymbol

-------------------------------------------------
-- New Loan Payment
-------------------------------------------------
-- | Information for a new loan payment.
data NewLoanPayment = NewLoanPayment
  -- | The current loan.
  { activeUTxO :: LoanUTxO
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The payment amount.
  , paymentAmount :: Text
  -- | The amount of each collateral asset being left, separated by newlines.
  , collateralBalances :: Text
  -- | Which network the loans are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewLoanPayment

instance Default NewLoanPayment where
  def = NewLoanPayment
    { activeUTxO = def
    , borrowerCredential = PubKeyCredential ""
    , stakeKeyDerivation = Nothing
    , paymentAmount = ""
    , collateralBalances = ""
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewLoanPayment`.
createNewLoanPayment
  :: ReverseTickerMap
  -> LoanUTxO -- ^ Current Active UTxO
  -> LoanWallet -- ^ Borrower's loan wallet.
  -> NewLoanPayment
createNewLoanPayment reverseTickerMap activeUTxO@LoanUTxO{lovelace,nativeAssets} borrowerWallet =
    def & #network .~ borrowerWallet ^. #network
        & #borrowerCredential .~ borrowerWallet ^. #stakeCredential
        & #alias .~ borrowerWallet ^. #alias
        & #stakeKeyDerivation .~ borrowerWallet ^. #stakeKeyDerivation
        & #collateralBalances .~ showCollateral lockedCollateral
        & #activeUTxO .~ activeUTxO
        & #paymentAmount .~ showAssetBalance True reverseTickerMap defaultPayment
  where 
    Loans.ActiveDatum{collateralization,loanAsset,minPayment,totalEpochPayments} = 
      fromMaybe def $ loanUTxOActiveDatum activeUTxO

    amountDue :: Integer
    amountDue = minPayment - totalEpochPayments

    defaultPayment :: NativeAsset
    defaultPayment 
      | amountDue > 0 = toNativeAsset loanAsset & #quantity .~ amountDue
      | otherwise = toNativeAsset loanAsset -- quantity 0

    adaIsCollateral :: Bool
    adaIsCollateral = any (== Loans.Asset ("","")) 
                    $ map fst 
                    $ Loans.unCollateralization collateralization

    allAssets
      | adaIsCollateral = (lovelaceAsNativeAsset & #quantity .~ unLovelace lovelace) : nativeAssets
      | otherwise = nativeAssets

    lockedCollateral = 
      filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets

    showCollateral = mconcat 
                   . intersperse "\n" 
                   . map (showAssetBalance True reverseTickerMap)

-------------------------------------------------
-- NewLoanPayment <--> LoanPayment
-------------------------------------------------
-- | Verify the user info for the new payment.
verifyNewLoanPayment
  :: ReverseTickerMap
  -> TickerMap 
  -> POSIXTime
  -> NewLoanPayment 
  -> Either Text LoanPayment
verifyNewLoanPayment reverseTickerMap tickerMap currentTime NewLoanPayment{..} = do
    -- Check that the assets are valid. Returns the first error, if any.
    verifiedCollateral <- mapM (parseNativeAssets tickerMap mempty) $ lines collateralBalances

    -- Check that the payment amount is valid. No fingerprints can be used.
    verifiedPaymentAmount <-
      first (const $ parseErrorMsg paymentAmount) $ parseNativeAssets tickerMap mempty paymentAmount

    when (verifiedPaymentAmount ^. #quantity <= 0) $
      Left "Loan payment amounts must be greater than 0."

    when (fromNativeAsset verifiedPaymentAmount /= loanAsset) $ Left $ unwords
      [ "The loan requires a payment in"
      , "'"
      , showAssetNameOnly reverseTickerMap (toNativeAsset loanAsset)
      , "'."
      ]

    let LoanUTxO{lovelace,nativeAssets} = activeUTxO
        startingCollateralValue = relativeCollateral $
          (lovelaceAsNativeAsset & #quantity .~ unLovelace lovelace) : nativeAssets
        endingCollateralValue = relativeCollateral verifiedCollateral
        startingBalance = toRational loanOutstanding
        payment = toRational $ verifiedPaymentAmount ^. #quantity
        collateralRatio = 
          abs (startingCollateralValue - endingCollateralValue) / startingCollateralValue
        paymentRatio = payment / startingBalance
        -- Collateral Taken / Starting Collateral <= Payment Amount / Starting Balance
        isValidCollateralization = collateralRatio <= paymentRatio

    -- Check that enough collateral is being supplied.
    unless isValidCollateralization $ Left $ unwords
      [ "Not enough collateral left after payment."
      , "You must leave at least"
      , displayPercentage (1 - paymentRatio) <> "%"
      , "of the relative collateral value."
      ]

    return $ LoanPayment
      { collateralBalances = verifiedCollateral
      , paymentAmount = verifiedPaymentAmount
      , calculatedCollateralDeposit = 0 -- This will be set later.
      , collateralDeposit = 0 -- This will be set later.
      , paymentDeposit = 0 -- This will be set later.
      , activeUTxO = activeUTxO
      , borrowerCredential = borrowerCredential
      , stakeKeyDerivation = stakeKeyDerivation
      , network = network
      , alias = alias
      , isFullPayment = payment >= startingBalance
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

    Loans.ActiveDatum{loanAsset,loanOutstanding,collateralization} = 
      fromMaybe def $ loanUTxOActiveDatum activeUTxO

    collateralRates :: Map.Map Loans.Asset Rational
    collateralRates = Map.fromList 
                    $ filter ((>0) . view _2)
                    $ map (over _2 toRational) 
                    $ Loans.unCollateralization collateralization

    relativeCollateral :: [NativeAsset] -> Rational
    relativeCollateral actualCollateral = go actualCollateral 0
      where
        go [] !acc = acc
        go (asset:as) !acc = case Map.lookup (fromNativeAsset asset) collateralRates of
          Nothing -> go as acc
          Just rate -> go as $ toRational (asset ^. #quantity) / rate + acc

toNewLoanPayment :: ReverseTickerMap -> LoanPayment -> NewLoanPayment
toNewLoanPayment reverseTickerMap LoanPayment{..} = NewLoanPayment
  { collateralBalances = unlines $ map (showAssetBalance True reverseTickerMap) collateralBalances
  , activeUTxO = activeUTxO
  , borrowerCredential = borrowerCredential
  , stakeKeyDerivation = stakeKeyDerivation
  , network = network
  , alias = alias
  , paymentAmount = showAssetBalance True reverseTickerMap paymentAmount
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Determine whether extra ada is needed to cover the minUTxOValue for either the collateral UTxO
-- or the lender payment UTxO. When ada is the loan asset, the loan payment amount must cover the
-- minUTxOValue value for the lender output so that it counts towards the borrower's balance.
updateLoanPaymentDeposits
  -- | There should either be 1 entry or 2 entries in the list. The first entry is always the
  -- lender payment minUTxO while the second entry is the collateral minUTxO. The collateral UTxO
  -- is not required for full payments.
  :: [Lovelace] 
  -> LoanPayment 
  -> ReverseTickerMap 
  -> Either Text LoanPayment
updateLoanPaymentDeposits minValues loanPayment reverseTickerMap = do
    let LoanPayment{isFullPayment, paymentAmount, collateralBalances} = loanPayment
    case minValues of
      [paymentDeposit] -> do
        unless isFullPayment $
          Left $ "calculateMinUTxOValue returned one result when it should return two"

        if paymentAmount ^. #policyId == "" then do
          -- The loan payment is ADA which means the minUTxOValue should be covered by the
          -- payment amount.
          when (paymentAmount ^. #quantity < unLovelace paymentDeposit) $ 
            Left $ unwords
              [ "The payment to the lender requires a minimum deposit of"
              , (showAssetBalance True reverseTickerMap $ paymentAmount & #quantity .~ 
                  unLovelace paymentDeposit) <> "."
              , "Make sure the loan payment is at least that amount so the deposit counts towards"
              , "your loan payment."
              ]

          return $ loanPayment
            & #collateralDeposit .~ 0
            & #calculatedCollateralDeposit .~ 0
            & #paymentDeposit .~ 0
        else
          -- The loan payment is not ADA which means the minUTxO must be included in addition
          -- to the loan payment.
          return $ loanPayment
            & #collateralDeposit .~ 0
            & #calculatedCollateralDeposit .~ 0
            & #paymentDeposit .~ paymentDeposit

      [paymentDeposit, collateralDeposit] -> do
        when isFullPayment $
          Left $ "calculateMinUTxOValue returned two results when it should only return one"

        let adaCollateral = fromMaybe lovelaceAsNativeAsset 
                          $ find ((== "") . view #policyId) collateralBalances
        if paymentAmount ^. #policyId == "" then do
          -- The loan payment is ADA which means the minUTxOValue should be covered by the
          -- payment amount.
          when (paymentAmount ^. #quantity < unLovelace paymentDeposit) $ 
            Left $ unwords
              [ "The payment to the lender requires a minimum deposit of"
              , (showAssetBalance True reverseTickerMap $ paymentAmount & #quantity .~ 
                  unLovelace paymentDeposit) <> "."
              , "Make sure the loan payment is at least that amount so the deposit counts towards"
              , "your loan payment."
              ]

          return $ loanPayment
            & #collateralDeposit .~ requiredCollateralDeposit collateralDeposit adaCollateral
            & #calculatedCollateralDeposit .~ collateralDeposit
            & #paymentDeposit .~ 0
        else
          -- The loan payment is not ADA which means the minUTxO must be included in addition
          -- to the loan payment.
          return $ loanPayment
            & #collateralDeposit .~ requiredCollateralDeposit collateralDeposit adaCollateral
            & #calculatedCollateralDeposit .~ collateralDeposit
            & #paymentDeposit .~ paymentDeposit

      _ -> Left "calculateMinUTxOValue returned wrong results"
  where
    -- | Calculate the extra required ada deposit for the collateral UTxO.
    requiredCollateralDeposit :: Lovelace -> NativeAsset -> Lovelace
    requiredCollateralDeposit calculatedDeposit adaValue =
      calculatedDeposit - Lovelace (adaValue ^. #quantity)

-- | Create the deposit message for the new loan payment.
createLoanPaymentDepositMsg :: LoanPayment -> Text
createLoanPaymentDepositMsg LoanPayment{..} = unlines $ intersperse "" $ filter (/= "")
    [ collateralDepositMsg
    , collateralChangeMsg
    , paymentMsg
    ]
  where
    collateralDepositMsg 
      | calculatedCollateralDeposit == 0 = mconcat
          [ "This is a full payment so all collateral and their deposit will be reclaimed." ]
      | otherwise = mconcat
          [ "The new collateral UTxO requires a deposit of: "
          , display calculatedCollateralDeposit
          , "."
          ]
    collateralChangeMsg
      | collateralDeposit == 0 = ""
      | collateralDeposit < 0 = mconcat
          [ "The deposit used for the previous collateral UTxO can be reused, and "
          , "you can reclaim the extra amount amount of: "
          , display $ abs collateralDeposit
          , "."
          ]
      | otherwise = mconcat
          [ "The deposit used for the previous collateral UTxO can be reused, but "
          , "you will need to cover the additional amount of: "
          , display collateralDeposit
          , "."
          ]
    paymentMsg
      | paymentDeposit == 0 = ""
      | otherwise = mconcat
          [ "Since the loan payment is not in ada, the payment output to the lender also "
          , "requires some ada to satisfy the minUTxOValue. The required amount is: "
          , display paymentDeposit
          , "."
          ]
