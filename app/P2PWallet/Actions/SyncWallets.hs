{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module P2PWallet.Actions.SyncWallets
  (
    syncWallets
  ) where

import UnliftIO.Async (pooledMapConcurrently)

import P2PWallet.Actions.Database
import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-- | This function gets the latest wallet states, and then backs up the states into the database.
syncWallets :: FilePath -> Network -> Wallets -> IO Wallets
syncWallets databaseFile network ws@Wallets{..} = do
  updatedPaymentWallets <- 
    pooledMapConcurrently (runQueryPaymentWalletInfo network) paymentWallets >>= 
      -- Throw an error if syncing failed.
      mapM fromRightOrAppError
  
  -- Save the new payment wallet states and throw an error if there is an issue saving.
  flip mapM_ updatedPaymentWallets $ \paymentWallet -> do
    addNewPaymentWallet databaseFile paymentWallet >>= fromRightOrAppError
    addNewTransactions databaseFile (paymentWallet ^. #transactions) >>= fromRightOrAppError

  updatedStakeWallets <- 
    pooledMapConcurrently (runQueryStakeWalletInfo network) stakeWallets >>= 
      -- Throw an error is syncing failed.
      mapM fromRightOrAppError

  -- Save the new stake wallet states and throw an error if there is an issue saving.
  flip mapM_ updatedStakeWallets $ \stakeWallet -> do
    addNewStakeWallet databaseFile stakeWallet >>= fromRightOrAppError
    addNewRewards databaseFile (stakeWallet ^. #rewardHistory) >>= fromRightOrAppError

  return $ 
    ws & #paymentWallets .~ updatedPaymentWallets
       & #stakeWallets .~ updatedStakeWallets
