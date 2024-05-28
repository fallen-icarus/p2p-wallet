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
  mapM_ (addNewPaymentWallet databaseFile >=> fromRightOrAppError) updatedPaymentWallets

  -- updatedStakeWallets <- 
  --   pooledMapConcurrently (runQueryStakeWalletInfo network') _stakeWallets >>= 
  --     mapM fromRightOrAppError

  return $ 
    ws & #paymentWallets .~ updatedPaymentWallets
       -- & #stakeWallets .~ updatedStakeWallets
