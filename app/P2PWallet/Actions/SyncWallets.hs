module P2PWallet.Actions.SyncWallets
  (
    syncWallets
  ) where

import UnliftIO.Async (pooledMapConcurrently, concurrently)

import P2PWallet.Actions.Database
import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Core.Internal.Notification
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-- | This function gets the latest wallet states, and then backs up the states into the database.
-- It will also get the current network parameters so they are available for transaction
-- building.
syncWallets :: FilePath -> Network -> Wallets -> IO (Wallets,(ByteString,Decimal),[Notification])
syncWallets databaseFile network ws@Wallets{..} = do
    -- The information is fetched concurrently.
    (updatedPaymentWallets,(updatedStakeWallets,networkParameters)) <- 
      concurrently fetchPaymentWallets $ concurrently fetchStakeWallets fetchParameters
      
    -- These are queried separately to minimize the chance of exceeding Koios' burst limit.
    (updatedDexWallets, updatedLoanWallets) <- concurrently fetchDexWallets fetchLoanWallets

    updatedOptionsWallets <- fetchOptionsWallets
    
    -- Save the new payment wallet states and throw an error if there is an issue saving.
    forM_ updatedPaymentWallets $ \paymentWallet -> do
      insertPaymentWallet databaseFile paymentWallet >>= fromRightOrAppError

    -- Save the new stake wallet states and throw an error if there is an issue saving.
    forM_ updatedStakeWallets $ \stakeWallet -> do
      insertStakeWallet databaseFile stakeWallet >>= fromRightOrAppError

    -- Save the new dex wallet states and throw an error if there is an issue saving.
    forM_ updatedDexWallets $ \dexWallet -> do
      insertDexWallet databaseFile dexWallet >>= fromRightOrAppError

    -- Save the new loan wallet states and throw an error if there is an issue saving.
    forM_ updatedLoanWallets $ \loanWallet -> do
      insertLoanWallet databaseFile loanWallet >>= fromRightOrAppError

    -- Save the new options wallet states and throw an error if there is an issue saving.
    forM_ updatedOptionsWallets $ \optionsWallet -> do
      insertOptionsWallet databaseFile optionsWallet >>= fromRightOrAppError

    let newNotifications = catMaybes $ mconcat
          [ zipWith notify paymentWallets updatedPaymentWallets
          , zipWith notify stakeWallets updatedStakeWallets
          , zipWith notify dexWallets updatedDexWallets
          , zipWith notify loanWallets updatedLoanWallets
          ]

    -- Return the updated wallets and network parameters.
    return $ (,networkParameters,newNotifications) $ ws
      & #paymentWallets .~ updatedPaymentWallets
      & #stakeWallets .~ updatedStakeWallets
      & #dexWallets .~ updatedDexWallets
      & #loanWallets .~ updatedLoanWallets
      & #optionsWallets .~ updatedOptionsWallets
  where
    fetchPaymentWallets :: IO [PaymentWallet]
    fetchPaymentWallets =
      pooledMapConcurrently runQueryPaymentWalletInfo paymentWallets >>= 
        -- Throw an error if syncing failed.
        mapM fromRightOrAppError

    fetchStakeWallets :: IO [StakeWallet]
    fetchStakeWallets =
      pooledMapConcurrently runQueryStakeWalletInfo stakeWallets >>= 
        -- Throw an error is syncing failed.
        mapM fromRightOrAppError

    fetchDexWallets :: IO [DexWallet]
    fetchDexWallets =
      pooledMapConcurrently runQueryDexWallet dexWallets >>= 
        -- Throw an error if syncing failed.
        mapM fromRightOrAppError

    fetchLoanWallets :: IO [LoanWallet]
    fetchLoanWallets =
      pooledMapConcurrently runQueryLoanWallet loanWallets >>= 
        -- Throw an error if syncing failed.
        mapM fromRightOrAppError

    fetchOptionsWallets :: IO [OptionsWallet]
    fetchOptionsWallets =
      pooledMapConcurrently runQueryOptionsWallet optionsWallets >>= 
        -- Throw an error if syncing failed.
        mapM fromRightOrAppError

    fetchParameters :: IO (ByteString, Decimal)
    fetchParameters = runGetParams network >>= fromRightOrAppError
