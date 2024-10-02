{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanKeyBurn where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Key Burn
-------------------------------------------------
-- | Information for burning a left over key nft.
data LoanKeyBurn = LoanKeyBurn
  -- | The key nft to burn.
  { loanIdAsset :: NativeAsset
  -- | Wallet burning the key.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanKeyBurn

instance AssetBalancesForChange (a,LoanKeyBurn) where
  assetBalancesForChange xs =
    ( 0
    , map (view $ _2 % #loanIdAsset) xs
    )

loanIdToLoanKeyBurn
  :: Network
  -> Text
  -> Loans.LoanId
  -> LoanKeyBurn
loanIdToLoanKeyBurn network alias (Loans.LoanId i) = 
  LoanKeyBurn
    { loanIdAsset = mkNativeAsset Loans.activeBeaconCurrencySymbol i & #quantity .~ (-1)
    , walletAlias = alias
    , network = network
    }
