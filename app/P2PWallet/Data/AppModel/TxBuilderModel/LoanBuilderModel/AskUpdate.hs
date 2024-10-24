{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskUpdate where

import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Ask Update
-------------------------------------------------
-- | An ask update is just the composition of closing one ask and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data AskUpdate = AskUpdate
  { oldAsk :: AskClose
  , newAsk :: AskCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AskUpdate

instance AssetBalancesForChange (a,AskUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldAsk) xs
    , assetBalancesForChange $ map (over _2 $ view #newAsk) xs
    ]

-- | Create a populated `NewAskCreation` based on the current ask terms.
loanUTxOToNewAskCreation 
  :: Network 
  -> Text 
  -> Credential 
  -> Maybe DerivationInfo 
  -> ReverseTickerMap 
  -> LoanUTxO 
  -> NewAskCreation
loanUTxOToNewAskCreation network alias borrowerCred keyDeriveInfo reverseTickerMap u@LoanUTxO{..} = 
    NewAskCreation
      { paymentAddress = loanAddress
      , borrowerCredential = borrowerCred
      , borrowerKeyDerivation = keyDeriveInfo
      , loanAmount = showAssetBalance True reverseTickerMap loanAmount
      , loanTerm = duration
      , collateral = unlines $ map (showAssetNameOnly reverseTickerMap) offeredCollateral
      , count = 1
      , network = network
      , alias = alias
      }
  where
    Loans.AskDatum{..} = fromMaybe def $ loanUTxOAskDatum u
    loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
    duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
    offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
