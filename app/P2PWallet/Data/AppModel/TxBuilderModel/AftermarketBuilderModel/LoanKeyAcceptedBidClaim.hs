{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeyAcceptedBidClaim where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.AcceptedBidClaim
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.ExpiredClaim
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Key Accepted Bid Claim
-------------------------------------------------
-- | Information for claiming an accepted bid for loan keys.
data LoanKeyAcceptedBidClaim = LoanKeyAcceptedBidClaim
  -- | The accepted bid being claim.
  { bidClaim :: AcceptedBidClaim
  -- | The new payment address for the loans.
  , newPaymentAddress :: Text
  -- | The address updates for the active loan UTxOs.
  , lenderAddressUpdates :: [LenderAddressUpdate]
  -- | The collateral claims for the expired loan UTxOs.
  , expiredClaims :: [ExpiredClaim]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanKeyAcceptedBidClaim

instance AssetBalancesForChange (a,LoanKeyAcceptedBidClaim) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #bidClaim) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #lenderAddressUpdates)) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #expiredClaims)) xs
    ]

-------------------------------------------------
-- NewLoanKeyAcceptedBidClaim
-------------------------------------------------
data NewLoanKeyAcceptedBidClaim = NewLoanKeyAcceptedBidClaim
  { bidClaim :: AcceptedBidClaim
  , newPaymentAddress :: Text
  , lenderAddressUpdates :: [NewLenderAddressUpdate]
  , expiredClaims :: [ExpiredClaim]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewLoanKeyAcceptedBidClaim

instance Default NewLoanKeyAcceptedBidClaim where
  def = NewLoanKeyAcceptedBidClaim
    { bidClaim = def
    , newPaymentAddress = ""
    , lenderAddressUpdates = []
    , expiredClaims = []
    }

-------------------------------------------------
-- NewLoanKeyAcceptedBidClaim <--> LoanKeyAcceptedBidClaim
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewLoanKeyAcceptedBidClaim 
  :: POSIXTime 
  -> NewLoanKeyAcceptedBidClaim 
  -> Either Text LoanKeyAcceptedBidClaim
verifyNewLoanKeyAcceptedBidClaim currentTime NewLoanKeyAcceptedBidClaim{..} = do
  verifiedLenderAddressUpdates <- 
    mapM 
      (verifyNewLenderAddressUpdate currentTime . set #newPaymentAddress newPaymentAddress) 
      lenderAddressUpdates

  return $ LoanKeyAcceptedBidClaim
    { bidClaim = bidClaim
    , newPaymentAddress = newPaymentAddress
    , lenderAddressUpdates = verifiedLenderAddressUpdates
    , expiredClaims = expiredClaims
    }

toNewLoanKeyAcceptedBidClaim :: LoanKeyAcceptedBidClaim -> NewLoanKeyAcceptedBidClaim
toNewLoanKeyAcceptedBidClaim LoanKeyAcceptedBidClaim{..} = NewLoanKeyAcceptedBidClaim
  { bidClaim = bidClaim
  , newPaymentAddress = newPaymentAddress
  , lenderAddressUpdates = map toNewLenderAddressUpdate lenderAddressUpdates
  , expiredClaims = expiredClaims
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateLoanKeyAcceptedBidClaimDeposits 
  :: LoanKeyAcceptedBidClaim 
  -> [Lovelace] 
  -> Either Text LoanKeyAcceptedBidClaim
updateLoanKeyAcceptedBidClaimDeposits i calculatedDeposits = do
  spotDeposit <- maybeToRight "calculatedDeposits is empty" $ maybeLast calculatedDeposits
  updateDeposits <- 
    fmap (groupInto 2) $
      maybeToRight "calculatedDeposits returned wrong results" $
        maybeInit calculatedDeposits

  newUpdates <- zipWithM updateLenderAddressDeposit (i ^. #lenderAddressUpdates) updateDeposits

  return $ i
    & #lenderAddressUpdates .~ newUpdates
    & #bidClaim %~ flip updateAcceptedBidClaimDeposit spotDeposit

-- | Generate the deposit message.
createLoanKeyAcceptedBidClaimDepositMsg :: LoanKeyAcceptedBidClaim -> Text
createLoanKeyAcceptedBidClaimDepositMsg LoanKeyAcceptedBidClaim{bidClaim,lenderAddressUpdates} =
  unlines $ intersperse "" $ filter (/= "") $
    [ createAcceptedBidClaimDepositMsg bidClaim ] <> 
      map createLenderAddressDepositMsg lenderAddressUpdates

-- | Generate the message about collateral being claimed.
createBidClaimImmediateExpiredLoansClaimMsg :: LoanKeyAcceptedBidClaim -> Text
createBidClaimImmediateExpiredLoansClaimMsg LoanKeyAcceptedBidClaim{expiredClaims}
  | null expiredClaims = ""
  | otherwise = "The collateral from the expired loans will be immediately claimed."
