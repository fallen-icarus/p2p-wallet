{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module P2PWallet.Actions.SyncWallets
  (
    syncWallets
  ) where

import UnliftIO.Async (pooledMapConcurrently)

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Lens
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

syncWallets :: Network -> Wallets -> IO Wallets
syncWallets network' ws@Wallets{..} = do
  updatedPaymentWallets <- 
    pooledMapConcurrently (runQueryPaymentWalletInfo network') _paymentWallets >>= 
      mapM fromRightOrAppError

  updatedStakeWallets <- 
    pooledMapConcurrently (runQueryStakeWalletInfo network') _stakeWallets >>= 
      mapM fromRightOrAppError

  return $ 
    ws & paymentWallets .~ updatedPaymentWallets
       & stakeWallets .~ updatedStakeWallets
