{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.ExpiredClaim where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Expired Claim
-------------------------------------------------
-- | Information for claiming expired collateral.
data ExpiredClaim = ExpiredClaim
  { loanUTxO :: LoanUTxO
  -- | The stake credential for the borrower if they are the ones claiming the collateral.
  -- This is Nothing if the lender is claiming the collateral.
  , borrowerCredential :: Maybe Credential
  -- | The path to the required hw key for witnessing.
  , borrowerStakeKeyDerivation :: Maybe DerivationInfo
  -- | Wallet claiming this UTxO.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The time of the claim.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ExpiredClaim

instance AssetBalancesForChange (a,ExpiredClaim) where
  assetBalancesForChange xs =
      ( sum $ map (view $ _2 % #loanUTxO % #lovelace) xs
      , normalAssets <> mapMaybe (extraLoanId . view _2) xs
      )
    where
      normalAssets :: [NativeAsset]
      normalAssets = filterOutBeacons $ concatMap (view $ _2 % #loanUTxO % #nativeAssets) xs

      extraLoanId :: ExpiredClaim -> Maybe NativeAsset
      extraLoanId ExpiredClaim{loanUTxO=LoanUTxO{loanDatum},borrowerCredential}
        -- The extra LoanId to burn is only required when the lender is claiming.
        | isJust borrowerCredential = Nothing
        | otherwise = (loanDatum ^? _Just % _ActiveDatum % #loanId) >>= 
            \(Loans.LoanId i) -> 
              Just $ mkNativeAsset Loans.activeBeaconCurrencySymbol i & #quantity .~ (-1)

      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= Loans.activeBeaconCurrencySymbol

loanUTxOToExpiredClaim
  :: Network
  -> Text
  -> Maybe Credential
  -> Maybe DerivationInfo
  -> POSIXTime
  -> LoanUTxO 
  -> ExpiredClaim
loanUTxOToExpiredClaim network alias mStakeCredential mKeyInfo currentTime loanUTxO = 
  ExpiredClaim
    { loanUTxO = loanUTxO
    , borrowerCredential = mStakeCredential
    , borrowerStakeKeyDerivation = mKeyInfo
    , walletAlias = alias
    , network = network
    , currentTime = toPlutusTime currentTime
    }
