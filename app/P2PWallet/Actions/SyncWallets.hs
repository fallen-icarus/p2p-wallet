module P2PWallet.Actions.SyncWallets
  (
    syncWallets
  ) where

import UnliftIO.Async (pooledMapConcurrently, concurrently)

import P2PWallet.Actions.Database
import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-- | This function gets the latest wallet states, and then backs up the states into the database.
-- It will also get the current network parameters so they are available for transaction
-- building.
syncWallets :: FilePath -> Network -> Wallets -> IO (Wallets,ByteString)
syncWallets databaseFile network ws@Wallets{..} = do
    -- The information is fetched concurrently.
    (updatedPaymentWallets,(updatedStakeWallets,(updatedDexWallets,networkParameters))) <- 
      concurrently fetchPaymentWallets $ 
        concurrently fetchStakeWallets $
          concurrently fetchDexWallets fetchParameters
    
    -- Save the new payment wallet states and throw an error if there is an issue saving.
    forM_ updatedPaymentWallets $ \paymentWallet -> do
      insertPaymentWallet databaseFile paymentWallet >>= fromRightOrAppError
      insertTransactions databaseFile (paymentWallet ^. #transactions) >>= fromRightOrAppError

    -- Save the new stake wallet states and throw an error if there is an issue saving.
    forM_ updatedStakeWallets $ \stakeWallet -> do
      insertStakeWallet databaseFile stakeWallet >>= fromRightOrAppError
      insertRewards databaseFile (stakeWallet ^. #rewardHistory) >>= fromRightOrAppError

    -- Save the new dex wallet states and throw an error if there is an issue saving.
    forM_ updatedDexWallets $ \dexWallet -> do
      insertDexWallet databaseFile dexWallet >>= fromRightOrAppError
      insertTransactions databaseFile (dexWallet ^. #transactions) >>= fromRightOrAppError

    -- Return the updated wallets and network parameters.
    return $ (,networkParameters) $ ws
      & #paymentWallets .~ updatedPaymentWallets
      & #stakeWallets .~ updatedStakeWallets
      & #dexWallets .~ updatedDexWallets
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

    fetchParameters :: IO ByteString
    fetchParameters = runGetParams network >>= fromRightOrAppError
