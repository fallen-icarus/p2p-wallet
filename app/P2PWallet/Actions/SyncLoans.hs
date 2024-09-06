module P2PWallet.Actions.SyncLoans
  (
    syncLoanAsks
  , syncLoanHistory
  , syncBorrowerInfo
  , syncLoanOffers
  , syncActiveLoans
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.LendingModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-- | This function gets all current loan asks that match the specified configuration.
syncLoanAsks :: Network -> LoanAskConfiguration -> CachedLoanAsks -> IO CachedLoanAsks
syncLoanAsks network askCfg currentCache = do
    allAsks <- runQueryLoanAsks network askCfg >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ currentCache
      & Map.insert askCfg allAsks

syncLoanHistory :: Network -> Loans.LoanId -> CachedLoanHistories -> IO CachedLoanHistories
syncLoanHistory network loanId currentCache = do
  (history,mUTxO) <- runQuerySpecificLoan network loanId >>= 
    -- Throw an error if syncing failed.
    fromRightOrAppError

  return $ currentCache
    & Map.insert loanId (history, mUTxO)

syncBorrowerInfo 
  :: Network 
  -> Loans.BorrowerId 
  -> PaymentAddress
  -> CachedBorrowerInfo
  -> IO CachedBorrowerInfo
syncBorrowerInfo network borrowerId borrowerAddr currentCache = do
  info <- runQueryBorrowerInformation network borrowerId borrowerAddr >>= 
    -- Throw an error if syncing failed.
    fromRightOrAppError

  return $ currentCache
    & Map.insert borrowerId info

-- | This function gets all current loan offers that match the specified configuration.
syncLoanOffers :: Network -> LoanOfferConfiguration -> CachedLoanOffers -> IO CachedLoanOffers
syncLoanOffers network offerCfg currentCache = do
    allOffers <- runQueryLoanOffers network offerCfg >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ insertIntoCachedOffers offerCfg allOffers currentCache

-- | This function gets all current active loans that match the specified configuration.
syncActiveLoans :: Network -> ActiveLoanConfiguration -> CachedActiveLoans -> IO CachedActiveLoans
syncActiveLoans network activeCfg currentCache = do
    allActives <- runQueryActiveLoans network activeCfg >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ insertIntoCachedActiveLoans activeCfg allActives currentCache
