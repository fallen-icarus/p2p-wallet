{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.BorrowerInformation where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Borrower Information
-------------------------------------------------
-- | The borrower information is the information a prospective lender sees when checking a
-- borrower's history. It shows the borrower's current credit history, as well as all current open
-- asks, all offers to them, and all of their current active loans.
data BorrowerInformation = BorrowerInformation
  { borrowerId :: Loans.BorrowerId
  , loanAddress :: PaymentAddress
  , creditHistory :: [LoanResult]
  , showCreditHistory :: Bool
  , openAsks :: [LoanUTxO]
  , showOpenAsks :: Bool
  , currentOffers :: [LoanUTxO]
  , showCurrentOffers :: Bool
  , activeLoans :: [LoanUTxO]
  , showActiveLoans :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BorrowerInformation

instance Default BorrowerInformation where
  def = BorrowerInformation
    { borrowerId = ""
    , loanAddress = ""
    , creditHistory = []
    , openAsks = []
    , currentOffers = []
    , activeLoans = []
    , showCreditHistory = False
    , showOpenAsks = False
    , showCurrentOffers = False
    , showActiveLoans = False
    }
