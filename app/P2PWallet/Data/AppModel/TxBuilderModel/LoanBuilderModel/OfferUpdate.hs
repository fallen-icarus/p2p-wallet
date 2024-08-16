{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferUpdate where

import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Update
-------------------------------------------------
-- | An offer update is just the composition of closing one offer and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data OfferUpdate = OfferUpdate
  { oldOffer :: OfferClose
  , newOffer :: OfferCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OfferUpdate

instance AssetBalancesForChange (a,OfferUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldOffer) xs
    , assetBalancesForChange $ map (over _2 $ view #newOffer) xs
    ]

-- | Create a populated `NewOfferCreation` based on the current offer terms.
loanUTxOToNewOfferCreation 
  :: POSIXTime
  -> Network 
  -> Text 
  -> Credential 
  -> Maybe DerivationInfo 
  -> ReverseTickerMap 
  -- | Either the first wallet in the tracked payment wallet list or the
  -- payment wallet already associated with the offer.
  -> PaymentWallet 
  -> LoanUTxO 
  -> NewOfferCreation
loanUTxOToNewOfferCreation 
  currentTime 
  network 
  alias 
  lenderCred 
  keyDeriveInfo 
  reverseTickerMap 
  wallet 
  u@LoanUTxO{..} = 
      NewOfferCreation
        { loanAddress = loanAddress
        , borrowerCredential = 
            fromRight (PubKeyCredential "") $ paymentAddressStakeCredential loanAddress
        , lenderCredential = lenderCred
        , lenderKeyDerivation = keyDeriveInfo
        , paymentWallet = wallet
        , loanAmount = showAssetBalance True reverseTickerMap loanAmount
        , loanTerm = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
        , interest = displayPercentage $ toRational loanInterest
        , compoundFrequency = maybe "" (show . calcDaysInPosixPeriod . fromPlutusTime) compoundFrequency
        , minPayment = showAssetQuantityOnly reverseTickerMap $ loanAmount & #quantity .~ minPayment
        , penalty = fromPenalty reverseTickerMap loanAmount penalty
        , claimPeriod = calcDaysInPosixPeriod $ fromPlutusTime claimPeriod
        , offerExpiration = 
            maybe "" (show . calcDaysInPosixPeriod . subtract currentTime . fromPlutusTime) offerExpiration
        , collateralization = unlines $ map displayCollateralization offeredCollateral
        , collateralIsSwappable = collateralIsSwappable
        , network = network
        , alias = alias
        }
  where
    Loans.OfferDatum{..} = fromMaybe def $ loanUTxOOfferDatum u
    offeredCollateral = map (over _1 toNativeAsset) 
                      $ map (over _2 toRational)
                      $ collateralization ^. #unCollateralization
    loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal

    displayCollateralization :: (NativeAsset,Rational) -> Text
    displayCollateralization (asset, price) = 
      showAssetNameOnly reverseTickerMap asset <> ", " <> 
        showPriceFormatted reverseTickerMap asset loanAmount price
