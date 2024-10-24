{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Address Update
-------------------------------------------------
-- | Information for updating a loan payment address.
data LenderAddressUpdate = LenderAddressUpdate
  { loanUTxO :: LoanUTxO
  -- | The wallet where future loan payments should go.
  , newPaymentWallet :: PaymentWallet
  -- | The extra deposit required for the new collateral UTxO.
  , extraDeposit :: Lovelace
  -- | The deposit that must go with the key NFT.
  , keyDeposit :: Lovelace
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The current time.
  , currentTime :: PlutusTime
  -- | The alias for the current holder.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LenderAddressUpdate

instance AssetBalancesForChange (a,LenderAddressUpdate) where
  assetBalancesForChange xs =
      -- Only the balance of ada can change and it can only increase by the extra amount required
      -- for the new minUTxOValue.
      ( sum
          [ sum $ map (negate . view (_2 % #extraDeposit)) xs
          , sum $ map (negate . view (_2 % #keyDeposit)) xs
          ]
      , map (extraLoanId . view _2) xs
      )
    where
      extraLoanId :: LenderAddressUpdate -> NativeAsset
      extraLoanId LenderAddressUpdate{loanUTxO=LoanUTxO{loanDatum}} =
        let (Loans.LoanId i) = fromMaybe "" $ loanDatum ^? _Just % _ActiveDatum % #loanId
         in mkNativeAsset Loans.activeBeaconCurrencySymbol i & #quantity .~ (-1)


-------------------------------------------------
-- New Address Update
-------------------------------------------------
-- | Information for updating a loan payment address.
data NewLenderAddressUpdate = NewLenderAddressUpdate
  { loanUTxO :: LoanUTxO
  , newPaymentWallet :: PaymentWallet
  -- | This is used internally to figure out which reference scripts to use.
  , network :: Network
  -- | The alias for the current holder.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewLenderAddressUpdate

instance Default NewLenderAddressUpdate where
  def = NewLenderAddressUpdate
    { loanUTxO = def
    , newPaymentWallet = def
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewLenderAddressUpdate`.
createNewLenderAddressUpdate
  :: Network
  -> PaymentWallet -- ^ The current holder's payment wallet.
  -> Text -- ^ The alias for the current holder.
  -> LoanUTxO
  -> NewLenderAddressUpdate
createNewLenderAddressUpdate network currentWallet alias loanUTxO =
    NewLenderAddressUpdate
      { loanUTxO = loanUTxO
      , network = network
      , newPaymentWallet = currentWallet
      , alias = alias
      }

-------------------------------------------------
-- NewLenderAddressUpdate <--> LenderAddressUpdate
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewLenderAddressUpdate :: POSIXTime -> NewLenderAddressUpdate -> Either Text LenderAddressUpdate
verifyNewLenderAddressUpdate currentTime NewLenderAddressUpdate{..} = do
  verifiedAddress <- parsePaymentAddress network $ display $ newPaymentWallet ^. #paymentAddress

  addrAsPlutus <- paymentAddressToPlutusAddress verifiedAddress

  unless (Loans.isValidLoanPaymentAddress addrAsPlutus) $
    Left $ unwords
      [ "Loan payment addresses must either use a payment pubkey, or the proxy script as the"
      , "payment credential and a staking credential."
      ]

  return $ LenderAddressUpdate
    { loanUTxO = loanUTxO
    , network = network
    , newPaymentWallet = newPaymentWallet
    , extraDeposit = 0 -- this will be set later.
    , keyDeposit = 0 -- this will be set later.
    , currentTime = toPlutusTime currentTime
    , alias = alias
    }

toNewLenderAddressUpdate :: LenderAddressUpdate -> NewLenderAddressUpdate
toNewLenderAddressUpdate LenderAddressUpdate{..} = NewLenderAddressUpdate
  { loanUTxO = loanUTxO
  , network = network
  , newPaymentWallet = newPaymentWallet
  , alias = alias
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateLenderAddressDeposit :: LenderAddressUpdate -> [Lovelace] -> Either Text LenderAddressUpdate
updateLenderAddressDeposit i@LenderAddressUpdate{loanUTxO=LoanUTxO{lovelace}} calculatedDeposits =
  case calculatedDeposits of
    [collateralDeposit, keyDeposit] -> 
      if collateralDeposit > lovelace then 
        return $ i 
          & #extraDeposit .~ collateralDeposit - lovelace
          & #keyDeposit .~ keyDeposit
      else 
        return $ i 
          & #extraDeposit .~ 0
          & #keyDeposit .~ keyDeposit
    _ -> Left "calculateMinUTxOValue did not return exactly two results"

-- | Generate the deposit message.
createLenderAddressDepositMsg :: LenderAddressUpdate -> Text
createLenderAddressDepositMsg LenderAddressUpdate{keyDeposit,extraDeposit} =
    unlines $ intersperse "" $ filter (/= "")
      [ keyDepositMsg
      , extraDepositMsg
      ]
  where
    keyDepositMsg = unwords
      [ "The Key NFT must be sent to the target address and stored with a deposit of"
      , display keyDeposit
      ]
    extraDepositMsg
      | extraDeposit <= 0 = ""
      | otherwise = unwords
          [ "The new collateral UTxO requires an extra"
          , display extraDeposit
          , "for the deposit."
          ]
