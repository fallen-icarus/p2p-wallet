{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeySpotPurchase where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotPurchase
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.ExpiredClaim
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Key Spot Purchase
-------------------------------------------------
-- | Information for purchasing a spot sale.
data LoanKeySpotPurchase = LoanKeySpotPurchase
  -- | The sale being purchased.
  { spotPurchase :: SpotPurchase
  -- | The new payment address for the active loans.
  , newPaymentWallet :: PaymentWallet
  -- | The address updates for the active loan UTxOs.
  , lenderAddressUpdates :: [LenderAddressUpdate]
  -- | The collateral claims for the expired loan UTxOs.
  , expiredClaims :: [ExpiredClaim]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanKeySpotPurchase

instance AssetBalancesForChange (a,LoanKeySpotPurchase) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #spotPurchase) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #lenderAddressUpdates)) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #expiredClaims)) xs
    ]

-------------------------------------------------
-- NewLoanKeySpotPurchase
-------------------------------------------------
data NewLoanKeySpotPurchase = NewLoanKeySpotPurchase
  { spotPurchase :: SpotPurchase
  , newPaymentWallet :: PaymentWallet
  , lenderAddressUpdates :: [NewLenderAddressUpdate]
  , expiredClaims :: [ExpiredClaim]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewLoanKeySpotPurchase

instance Default NewLoanKeySpotPurchase where
  def = NewLoanKeySpotPurchase
    { spotPurchase = def
    , newPaymentWallet = def
    , lenderAddressUpdates = []
    , expiredClaims = []
    }

-------------------------------------------------
-- NewLoanKeySpotPurchase <--> LoanKeySpotPurchase
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewLoanKeySpotPurchase :: POSIXTime -> NewLoanKeySpotPurchase -> Either Text LoanKeySpotPurchase
verifyNewLoanKeySpotPurchase currentTime NewLoanKeySpotPurchase{..} = do
  verifiedLenderAddressUpdates <- 
    mapM 
      (verifyNewLenderAddressUpdate currentTime . set #newPaymentWallet newPaymentWallet) 
      lenderAddressUpdates

  return $ LoanKeySpotPurchase
    { spotPurchase = spotPurchase
    , newPaymentWallet = newPaymentWallet
    , lenderAddressUpdates = verifiedLenderAddressUpdates
    , expiredClaims = expiredClaims
    }

toNewLoanKeySpotPurchase :: LoanKeySpotPurchase -> NewLoanKeySpotPurchase
toNewLoanKeySpotPurchase LoanKeySpotPurchase{..} = NewLoanKeySpotPurchase
  { spotPurchase = spotPurchase
  , newPaymentWallet = newPaymentWallet
  , lenderAddressUpdates = map toNewLenderAddressUpdate lenderAddressUpdates
  , expiredClaims = expiredClaims
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateLoanKeySpotPurchaseDeposits :: LoanKeySpotPurchase -> [Lovelace] -> Either Text LoanKeySpotPurchase
updateLoanKeySpotPurchaseDeposits i calculatedDeposits = do
  spotDeposit <- maybeToRight "calculatedDeposits is empty" $ maybeLast calculatedDeposits
  updateDeposits <- 
    fmap (groupInto 2) $
      maybeToRight "calculatedDeposits returned wrong results" $
        maybeInit calculatedDeposits

  newUpdates <- zipWithM updateLenderAddressDeposit (i ^. #lenderAddressUpdates) updateDeposits

  return $ i
    & #lenderAddressUpdates .~ newUpdates
    & #spotPurchase %~ flip updateSpotPurchaseDeposit spotDeposit

-- | Generate the deposit message.
createLoanKeySpotPurchaseDepositMsg :: LoanKeySpotPurchase -> Text
createLoanKeySpotPurchaseDepositMsg LoanKeySpotPurchase{spotPurchase,lenderAddressUpdates} =
  unlines $ intersperse "" $ filter (/= "") $
    [ createSpotPurchaseDepositMsg spotPurchase ] <> 
      map createLenderAddressDepositMsg lenderAddressUpdates

-- | Generate the message about collateral being claimed.
createSpotPurchaseImmediateExpiredLoansClaimMsg :: LoanKeySpotPurchase -> Text
createSpotPurchaseImmediateExpiredLoansClaimMsg LoanKeySpotPurchase{expiredClaims}
  | null expiredClaims = ""
  | otherwise = "The collateral from the expired loans will be immediately claimed."
