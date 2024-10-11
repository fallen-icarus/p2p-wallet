{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferAcceptance where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Acceptance
-------------------------------------------------
-- | Information for accepting an offer.
data OfferAcceptance = OfferAcceptance
  -- | The offer being accepted.
  { offerUTxO :: LoanUTxO
  -- | The ask UTxO to close. This can be an ask for different terms than the offer.
  , askUTxO :: LoanUTxO
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of each collateral asset being used.
  , collateralAmounts :: [NativeAsset]
  -- | The minUTxOValue amount of ada required for the new active UTxO.
  , deposit :: Lovelace
  -- | Which network the loans are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , alias :: Text
  -- | Current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OfferAcceptance

instance AssetBalancesForChange (a,OfferAcceptance) where
  assetBalancesForChange xs =
      ( sum 
          [ sum $ map (view $ _2 % #offerUTxO % #lovelace) xs
          , sum $ map (view $ _2 % #askUTxO % #lovelace) xs
          , sum $ map (negate . requiredDeposit . view _2) xs
          , sum $ for xs $ \accept ->
              maybe 0 (Lovelace . negate) $ 
                accept ^? _2 % #offerUTxO % #loanDatum % _Just % _OfferDatum % #offerDeposit
          ]
      , filterOutBeacons $ sumNativeAssets $ mconcat
          [ concatMap (view $ _2 % #offerUTxO % #nativeAssets) xs
          , concatMap (view $ _2 % #askUTxO % #nativeAssets) xs
          , filter ((/= "") . view #policyId) $ map (over #quantity negate) $
              concatMap (view $ _2 % #collateralAmounts) xs
          ]
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= Loans.negotiationBeaconCurrencySymbol

      -- Since ada could be the collateral, the additional ada for the
      -- collateral minUTxOValue may not be necessary.
      requiredDeposit :: OfferAcceptance -> Lovelace
      requiredDeposit OfferAcceptance{deposit, collateralAmounts} =
        let collateralLovelace = Lovelace 
                               $ view #quantity 
                               $ fromMaybe lovelaceAsNativeAsset 
                               $ find ((=="") . view #policyId) collateralAmounts
         in if deposit - collateralLovelace <= 0 then collateralLovelace else deposit

-------------------------------------------------
-- New Offer Acceptance
-------------------------------------------------
-- | Information for accepting an offer.
data NewOfferAcceptance = NewOfferAcceptance
  -- | The offer being accepted.
  { offerUTxO :: LoanUTxO
  -- | The ask UTxO to close. This can be an ask for different terms than the offer.
  , askUTxO :: LoanUTxO
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of each collateral asset being used.
  , collateralAmounts :: Text
  -- | Which network the loans are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewOfferAcceptance

instance Default NewOfferAcceptance where
  def = NewOfferAcceptance
    { offerUTxO = def
    , askUTxO = def
    , borrowerCredential = PubKeyCredential ""
    , stakeKeyDerivation = Nothing
    , collateralAmounts = ""
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewOfferAcceptance`.
createNewOfferAcceptance
  :: ReverseTickerMap
  -> LoanUTxO -- ^ Offer UTxO
  -> LoanUTxO -- ^ First Ask UTxO available.
  -> LoanWallet -- ^ Borrower's loan wallet.
  -> NewOfferAcceptance
createNewOfferAcceptance reverseTickerMap offerUTxO firstAskUTxO borrowerWallet =
    def & #network .~ borrowerWallet ^. #network
        & #borrowerCredential .~ borrowerWallet ^. #stakeCredential
        & #alias .~ borrowerWallet ^. #alias
        & #stakeKeyDerivation .~ borrowerWallet ^. #stakeKeyDerivation
        & #collateralAmounts .~ showCollateralization collateralization
        & #offerUTxO .~ offerUTxO
        & #askUTxO .~ firstAskUTxO
  where 
    offerDatum :: Loans.OfferDatum
    offerDatum = fromMaybe def $ loanUTxOOfferDatum offerUTxO

    collateralization :: [NativeAsset]
    collateralization = map (toNativeAsset . fst)
                      $ offerDatum ^. #collateralization % #unCollateralization

    showCollateralization = mconcat . intersperse "\n" 
                          . map (showAssetBalance True reverseTickerMap)

-------------------------------------------------
-- NewOfferAcceptance <--> OfferAcceptance
-------------------------------------------------
-- | Verify the user info.
verifyNewOfferAcceptance
  :: TickerMap 
  -> POSIXTime -- ^ Current time.
  -> NewOfferAcceptance 
  -> Either Text OfferAcceptance
verifyNewOfferAcceptance tickerMap currentTime NewOfferAcceptance{..} = do
    -- Check that the assets are valid. Returns the first error, if any.
    verifiedCollateral <- mapM (parseNativeAssets tickerMap mempty) $ lines collateralAmounts

    -- Check that enough collateral is being supplied.
    when (toRational loanPrincipal > relativeCollateral verifiedCollateral) $
      Left "Not enough collateral specified."

    return $ OfferAcceptance
      { collateralAmounts = verifiedCollateral
      , deposit = 0 -- This will be set later.
      , offerUTxO = offerUTxO
      , askUTxO = askUTxO
      , borrowerCredential = borrowerCredential
      , stakeKeyDerivation = stakeKeyDerivation
      , network = network
      , alias = alias
      , currentTime = toPlutusTime currentTime
      }
  where
    Loans.OfferDatum{loanPrincipal,collateralization=Loans.Collateralization cs} = 
      fromMaybe def $ loanUTxOOfferDatum offerUTxO

    collateralRates :: Map.Map Loans.Asset Rational
    collateralRates = Map.fromList 
                    $ filter ((>0) . view _2)
                    $ map (over _2 toRational) cs

    relativeCollateral :: [NativeAsset] -> Rational
    relativeCollateral actualCollateral = go actualCollateral 0
      where
        go [] !acc = acc
        go (asset:as) !acc = case Map.lookup (fromNativeAsset asset) collateralRates of
          Nothing -> go as acc
          Just rate -> go as $ toRational (asset ^. #quantity) / rate + acc

toNewOfferAcceptance :: ReverseTickerMap -> OfferAcceptance -> NewOfferAcceptance
toNewOfferAcceptance reverseTickerMap OfferAcceptance{..} = NewOfferAcceptance
  { collateralAmounts = unlines $ map (showAssetBalance True reverseTickerMap) collateralAmounts
  , offerUTxO = offerUTxO
  , askUTxO = askUTxO
  , borrowerCredential = borrowerCredential
  , stakeKeyDerivation = stakeKeyDerivation
  , network = network
  , alias = alias
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | When ada is used as collateral, it must at least cover the minUTxOValue.
acceptanceAdaCollateralCheck :: OfferAcceptance -> Either Text ()
acceptanceAdaCollateralCheck OfferAcceptance{offerUTxO, deposit, collateralAmounts} = do
  when (requiredDeposit > 0 && usesAdaAsCollateral) $
    Left $ unwords
      [ "The collateral UTxO must be stored with at least " <> display deposit <> " in order"
      , "to satisfy the minUTxOValue requirement.\n\n"
      , "Since Cardano forces all UTxOs to contain ada and this lender is willing to accept ada"
      , "as collateral, please use at least " <> display deposit <> " as collateral"
      , "so that the minUTxOValue deposit will count towards your collateral."
      ]
  where
    collateralLovelace = Lovelace 
                       $ view #quantity 
                       $ fromMaybe lovelaceAsNativeAsset 
                       $ find ((=="") . view #policyId) collateralAmounts

    usesAdaAsCollateral = any (\(asset,price) -> asset == Loans.Asset ("","") && price > 0)
                        $ view (#collateralization % #unCollateralization)
                        $ fromMaybe def
                        $ loanUTxOOfferDatum offerUTxO

    -- Since ada could be the collateral, the additional ada for the
    -- collateral minUTxOValue may not be necessary.
    requiredDeposit :: Lovelace
    requiredDeposit = deposit - collateralLovelace

-- | Create the message for the offer acceptance, accounting for whether the ada collateral
-- covers the minUTxOValue requirement.
createAcceptanceDepositMsg :: OfferAcceptance -> Text
createAcceptanceDepositMsg OfferAcceptance{deposit, collateralAmounts}
  | requiredDeposit > 0 = unwords
      [ "This new collateral output requires a deposit of: " <> display deposit <> "."
      , "This deposit is in addition to the collateral since all Cardano UTxOs must contain"
      , "a minimum amount of ada based on the size of the UTxO."
      ]
  | otherwise = "The ada collateral covers the deposit requirement."
  where
    collateralLovelace = Lovelace 
                       $ view #quantity 
                       $ fromMaybe lovelaceAsNativeAsset 
                       $ find ((=="") . view #policyId) collateralAmounts

    -- Since ada could be the collateral, the additional ada for the
    -- collateral minUTxOValue may not be necessary.
    requiredDeposit :: Lovelace
    requiredDeposit = deposit - collateralLovelace
