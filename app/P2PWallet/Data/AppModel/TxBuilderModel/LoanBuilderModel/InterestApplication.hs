{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.InterestApplication where

import Data.Maybe (fromJust)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Interest/Penalty Application
-------------------------------------------------
-- | Information for an interest application. The number of interest applications required
-- and the required deposit increase are automatically determined.
data InterestApplication = InterestApplication
  -- | The current loan.
  { utxoRef :: TxOutRef
  -- | The loan address.
  , loanAddress :: PaymentAddress
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The current value of ada.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current loan terms.
  , activeDatum :: Loans.ActiveDatum
  -- | The calculated extra deposit.
  , extraDeposit :: Lovelace
  -- | Number of applications required.
  , requiredApplicationCount :: Integer
  -- | Which network the loans are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , alias :: Text
  -- | The current time. This is used to determine the invalid-hereafter bound. As well as how
  -- many interest/penalty applications are required.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''InterestApplication

instance AssetBalancesForChange (a,InterestApplication) where
  assetBalancesForChange xs =
      -- Only the balance of ada can change and it can only increase by the extra amount required
      -- for the new minUTxOValue.
      ( sum $ map (negate . view (_2 % #extraDeposit)) xs
      , [] -- The native asset balances do not change during an interest application.
      )

loanUTxOToInterestApplication
  :: Network
  -> Text
  -> Credential
  -> Maybe DerivationInfo
  -> POSIXTime
  -> LoanUTxO
  -> InterestApplication
loanUTxOToInterestApplication network alias stakeCredential mKeyInfo currentTime u@LoanUTxO{..} =
    InterestApplication
      { utxoRef = utxoRef
      , loanAddress = loanAddress
      , borrowerCredential = stakeCredential
      , stakeKeyDerivation = mKeyInfo
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , activeDatum = fromMaybe def $ loanUTxOActiveDatum u
      , alias = alias
      , network = network
      , currentTime = toPlutusTime currentTime
      , extraDeposit = 0 -- This will be set later.
      , requiredApplicationCount = toInteger $
          -- The GUI should prevent applying interests/penalties on loans with no compoundFrequency.
          (toPlutusTime currentTime - lastCompounding) `div` fromJust compoundFrequency 
      }
  where 
    Loans.ActiveDatum{lastCompounding, compoundFrequency} = 
      fromMaybe def $ loanUTxOActiveDatum u

-- | Update the required deposit increase if necessary.
updateInterestDeposit :: InterestApplication -> Lovelace -> InterestApplication
updateInterestDeposit i@InterestApplication{lovelace} calculatedDeposit
  | calculatedDeposit > lovelace = i & #extraDeposit .~ calculatedDeposit - lovelace
  | otherwise = i & #extraDeposit .~ 0

-- | Generate the deposit message.
createInterestDepositMsg :: InterestApplication -> Text
createInterestDepositMsg InterestApplication{requiredApplicationCount,extraDeposit} =
    unlines $ intersperse "" $ filter (/= "")
      [ applicationCountMsg
      , extraDepositMsg
      ]
  where
    applicationCountMsg = unwords
      [ "The interest (and any penalties) will be applied"
      , show requiredApplicationCount
      , "time(s)."
      ]
    extraDepositMsg
      | extraDeposit <= 0 = ""
      | otherwise = unwords
          [ "The new collateral UTxO requires an extra"
          , display extraDeposit
          , "for the deposit."
          ]
