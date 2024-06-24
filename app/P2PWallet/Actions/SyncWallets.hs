module P2PWallet.Actions.SyncWallets
  (
    syncWallets
  ) where

import UnliftIO.Async (pooledMapConcurrently)

import P2PWallet.Actions.Database
import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-- | This function gets the latest wallet states, and then backs up the states into the database.
syncWallets :: FilePath -> Wallets -> IO Wallets
syncWallets databaseFile ws@Wallets{..} = do
  updatedPaymentWallets <-
    pooledMapConcurrently runQueryPaymentWalletInfo paymentWallets >>= 
      -- Throw an error if syncing failed.
      mapM fromRightOrAppError
  
  -- Save the new payment wallet states and throw an error if there is an issue saving.
  forM_ updatedPaymentWallets $ \paymentWallet -> do
    insertPaymentWallet databaseFile paymentWallet >>= fromRightOrAppError
    insertTransactions databaseFile (paymentWallet ^. #transactions) >>= fromRightOrAppError

  updatedStakeWallets <- 
    pooledMapConcurrently runQueryStakeWalletInfo stakeWallets >>= 
      -- Throw an error is syncing failed.
      mapM fromRightOrAppError

  -- Save the new stake wallet states and throw an error if there is an issue saving.
  forM_ updatedStakeWallets $ \stakeWallet -> do
    insertStakeWallet databaseFile stakeWallet >>= fromRightOrAppError
    insertRewards databaseFile (stakeWallet ^. #rewardHistory) >>= fromRightOrAppError

  return $ 
    ws & #paymentWallets .~ updatedPaymentWallets
       & #stakeWallets .~ updatedStakeWallets
