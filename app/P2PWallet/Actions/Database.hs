{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module P2PWallet.Actions.Database
  ( 
    initializeDatabase
  , loadProfiles
  , addNewProfile
  , deleteProfile
  , getNextProfileId
  , getNextPaymentId
  , addNewPaymentWallet
  , loadWallets
  , deletePaymentWallet
  , addNewTransactions
  , loadTransactions
  , getNextStakeId
  , loadStakeWallets
  , deleteStakeWallet
  , addNewStakeWallet
  , addNewRewards
  , getNextAddressEntryId
  , addNewAddressEntry
  , loadAddressBook
  , deleteAddressEntry
  , addNewTickerInfo
  , loadTickerInfo
  , deleteTickerInfo
  ) where

import System.Directory qualified as Dir
import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Actions.Utils
import P2PWallet.Data.AddressBook
import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Data.Profile
import P2PWallet.Data.TickerMap
import P2PWallet.Data.Transaction
import P2PWallet.Data.StakeReward
import P2PWallet.Data.Wallets

import P2PWallet.Prelude

-------------------------------------------------
-- Create a new database
-------------------------------------------------
initializeDatabase :: FilePath -> IO (Either Text ())
initializeDatabase dbFile = do
  -- Check if the database already exists.
  exists <- Dir.doesFileExist dbFile

  if exists then return $ Right () else do
    handle @SomeException (return . Left . ("Could not create database: " <>) . show) $ do
      -- Create the tables in the database.
      create @Profile dbFile
      create @PaymentWallet dbFile
      create @StakeReward dbFile
      create @StakeWallet dbFile
      create @Transaction dbFile
      create @AddressEntry dbFile
      create @TickerInfo dbFile
      return $ Right ()

-------------------------------------------------
-- Profiles
-------------------------------------------------
-- | Load the profiles for the specified network. If the sqlite file does not exist, create a new
-- one and return and empty list.
loadProfiles :: FilePath -> Network -> IO (Either Text [Profile])
loadProfiles dbFile network = do
    handle @SomeException (return . Left . ("Could not load profiles: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @Profile
      , "WHERE network = " <> show (toString network) -- it must be wrapped in quotes
      ]

-- | Add a new profile to the database. This also updates profiles.
addNewProfile :: FilePath -> Profile -> IO (Either Text ())
addNewProfile dbFile profile = do
  handle @SomeException (return . Left . ("Failed to insert profile: " <>) . show) $
    Right <$> insert @Profile dbFile profile

deleteProfile :: FilePath -> ProfileId -> IO (Either Text ())
deleteProfile dbFile (ProfileId profileId) = 
    handle @SomeException (return . Left . ("Failed to delete profile: " <>) . show) $ do
      delete dbFile deleteProfileStmt
      delete dbFile deletePaymentWalletStmt
      delete dbFile deleteTxStmt
      return $ Right ()
  where
    deleteProfileStmt :: Query
    deleteProfileStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @Profile
      , "WHERE profile_id = " <> show profileId
      ]

    deletePaymentWalletStmt :: Query
    deletePaymentWalletStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @PaymentWallet
      , "WHERE profile_id = " <> show profileId
      ]

    deleteTxStmt :: Query
    deleteTxStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @Transaction
      , "WHERE profile_id = " <> show profileId
      ]

getNextProfileId :: FilePath -> IO (Either Text ProfileId)
getNextProfileId dbFile =
    handle @SomeException (return . Left . ("Could not get next profile id: " <>) . show) $
      -- If the result is the empty list, this is the first profile entry.
      maybe (Right 0) (Right . ProfileId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT profile_id FROM"
      , tableName @Profile
      , "ORDER BY profile_id DESC"
      , "LIMIT 1;"
      ]

-- | Load the wallets for the specified profile.
loadWallets :: FilePath -> Profile -> IO (Either Text Wallets)
loadWallets dbFile Profile{..} = do
  handle @SomeException (return . Left . ("Could not load wallets: " <>) . show) $ do
    -- Load the payment wallets.
    paymentWalletsWithoutTxHistories <- loadPaymentWallets dbFile profileId >>= fromRightOrAppError

    -- Load the transaction histories for each payment wallet.
    paymentWalletsWithTxHistories <- flip mapM paymentWalletsWithoutTxHistories $ 
      \paymentWallet -> do
        txs <- loadTransactions dbFile (paymentWallet ^. #paymentId) >>= fromRightOrAppError
        return $ paymentWallet & #transactions .~ txs

    -- Load the stake wallets.
    stakeWalletsWithoutRewardHistory <- loadStakeWallets dbFile profileId >>= fromRightOrAppError

    -- Load the reward histories for each stake wallet.
    stakeWalletsWithRewardHistory <- flip mapM stakeWalletsWithoutRewardHistory $ 
      \stakeWallet -> do
        rewards <- loadRewards dbFile (stakeWallet ^. #stakeId) >>= fromRightOrAppError
        return $ stakeWallet & #rewardHistory .~ rewards

    return $ Right $ Wallets
      { paymentWallets = paymentWalletsWithTxHistories
      , stakeWallets = stakeWalletsWithRewardHistory
      }

-------------------------------------------------
-- Payment Wallet
-------------------------------------------------
getNextPaymentId :: FilePath -> IO (Either Text PaymentId)
getNextPaymentId dbFile =
    handle @SomeException (return . Left . ("Could not get next payment id: " <>) . show) $
      -- If the result is the empty list, this is the first profile entry.
      maybe (Right 0) (Right . PaymentId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT payment_id FROM"
      , tableName @PaymentWallet
      , "ORDER BY payment_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new payment wallet to the database. This also updates payment wallet.
addNewPaymentWallet :: FilePath -> PaymentWallet -> IO (Either Text ())
addNewPaymentWallet dbFile paymentWallet = do
  handle @SomeException (return . Left . ("Failed to insert payment wallet: " <>) . show) $
    Right <$> insert @PaymentWallet dbFile paymentWallet


-- | Load the payment wallets for the specified profile
loadPaymentWallets :: FilePath -> ProfileId -> IO (Either Text [PaymentWallet])
loadPaymentWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load payment wallets : " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @PaymentWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY payment_id ASC;"
      ]

deletePaymentWallet :: FilePath -> PaymentId -> IO (Either Text ())
deletePaymentWallet dbFile (PaymentId paymentId) = 
  handle @SomeException (return . Left . ("Failed to delete payment wallet: " <>) . show) $ do
    delete dbFile deletePaymentWalletStmt
    delete dbFile deleteTxStmt
    return $ Right ()
  where
    deletePaymentWalletStmt :: Query
    deletePaymentWalletStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @PaymentWallet
      , "WHERE payment_id = " <> show paymentId
      ]

    deleteTxStmt :: Query
    deleteTxStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @Transaction
      , "WHERE payment_id = " <> show paymentId
      ]

-------------------------------------------------
-- Transactions
-------------------------------------------------
-- | Add a new transaction to the database.
addNewTransactions :: FilePath -> [Transaction] -> IO (Either Text ())
addNewTransactions dbFile txs = do
  handle @SomeException (return . Left . ("Failed to insert transactions: " <>) . show) $
    Right <$> insertMany @Transaction dbFile txs

-- | Load the transactions for the specified payment wallet.
loadTransactions :: FilePath -> PaymentId -> IO (Either Text [Transaction])
loadTransactions dbFile (PaymentId paymentId) = do
    handle @SomeException (return . Left . ("Could not load transactions: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @Transaction
      , "WHERE payment_id = " <> show paymentId
      , "ORDER BY block_height DESC;"
      ]

-------------------------------------------------
-- Stake Wallet
-------------------------------------------------
getNextStakeId :: FilePath -> IO (Either Text StakeId)
getNextStakeId dbFile =
    handle @SomeException (return . Left . ("Could not get next stake id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . StakeId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT stake_id FROM"
      , tableName @StakeWallet
      , "ORDER BY stake_id DESC"
      , "LIMIT 1;"
      ]

-- | Load the stake wallets for the specified profile
loadStakeWallets :: FilePath -> ProfileId -> IO (Either Text [StakeWallet])
loadStakeWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load stake wallets : " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @StakeWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY stake_id ASC;"
      ]

-- | Add a new stake wallet to the database. This also updates stake wallets.
addNewStakeWallet :: FilePath -> StakeWallet -> IO (Either Text ())
addNewStakeWallet dbFile stakeWallet = do
  handle @SomeException (return . Left . ("Failed to insert stake wallet: " <>) . show) $
    Right <$> insert @StakeWallet dbFile stakeWallet

deleteStakeWallet :: FilePath -> StakeId -> IO (Either Text ())
deleteStakeWallet dbFile (StakeId stakeId) = 
  handle @SomeException (return . Left . ("Failed to delete stake wallet: " <>) . show) $ do
    delete dbFile deleteStakeWalletStmt
    delete dbFile deleteRewardStmt
    return $ Right ()
  where
    deleteStakeWalletStmt :: Query
    deleteStakeWalletStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @StakeWallet
      , "WHERE stake_id = " <> show stakeId
      ]

    deleteRewardStmt :: Query
    deleteRewardStmt = Query $ unwords
      [ "DELETE FROM " <> tableName @StakeReward
      , "WHERE stake_id = " <> show stakeId
      ]

-------------------------------------------------
-- Stake Rewards
-------------------------------------------------
-- | Add a new stake reward to the database.
addNewRewards :: FilePath -> [StakeReward] -> IO (Either Text ())
addNewRewards dbFile rs = do
  handle @SomeException (return . Left . ("Failed to insert rewards: " <>) . show) $
    Right <$> insertMany @StakeReward dbFile rs

-- | Load the rewards for the specified stake wallet.
loadRewards :: FilePath -> StakeId -> IO (Either Text [StakeReward])
loadRewards dbFile (StakeId stakeId) = do
    handle @SomeException (return . Left . ("Could not load rewards: " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @StakeReward
      , "WHERE stake_id = " <> show stakeId
      , "ORDER BY earned_epoch DESC;"
      ]

-------------------------------------------------
-- Address Book
-------------------------------------------------
getNextAddressEntryId :: FilePath -> IO (Either Text AddressEntryId)
getNextAddressEntryId dbFile =
    handle @SomeException (return . Left . ("Could not get next address entry id: " <>) . show) $
      -- If the result is the empty list, this is the first profile entry.
      maybe (Right 0) (Right . AddressEntryId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT address_entry_id FROM"
      , tableName @AddressEntry
      , "ORDER BY address_entry_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new address entry to the database.
addNewAddressEntry :: FilePath -> [AddressEntry] -> IO (Either Text ())
addNewAddressEntry dbFile entries = do
  handle @SomeException (return . Left . ("Failed to insert address entry: " <>) . show) $
    fmap sequence_ $ mapM (fmap Right . insert @AddressEntry dbFile) entries

-- | Load the address book for the specified profile id.
loadAddressBook :: FilePath -> Profile -> IO (Either Text [AddressEntry])
loadAddressBook dbFile Profile{profileId=ProfileId profileId} = do
    handle @SomeException (return . Left . ("Could not load address book: " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @AddressEntry
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY alias ASC;"
      ]

deleteAddressEntry :: FilePath -> AddressEntryId -> IO (Either Text ())
deleteAddressEntry dbFile (AddressEntryId entryId) = 
  handle @SomeException (return . Left . ("Failed to delete address entry: " <>) . show) $ do
    delete dbFile deleteSmt
    return $ Right ()
  where
    deleteSmt :: Query
    deleteSmt = Query $ unwords
      [ "DELETE FROM " <> tableName @AddressEntry
      , "WHERE address_entry_id = " <> show entryId
      ]

-------------------------------------------------
-- Ticker Map
-------------------------------------------------
-- | Add a new ticker to the database.
addNewTickerInfo :: FilePath -> TickerInfo -> IO (Either Text ())
addNewTickerInfo dbFile newTicker = do
  handle @SomeException (return . Left . ("Failed to insert ticker info: " <>) . show) $
    fmap Right $ insert @TickerInfo dbFile newTicker

-- | Load the ticker map. The ticker map is the same for all networks and profiles.
loadTickerInfo :: FilePath -> IO (Either Text [TickerInfo])
loadTickerInfo dbFile = do
    handle @SomeException (return . Left . ("Could not load ticker map: " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @TickerInfo
      ]

deleteTickerInfo :: FilePath -> Text -> IO (Either Text ())
deleteTickerInfo dbFile ticker = 
  handle @SomeException (return . Left . ("Failed to delete ticker info: " <>) . show) $ do
    delete dbFile deleteSmt
    return $ Right ()
  where
    deleteSmt :: Query
    deleteSmt = Query $ unwords
      [ "DELETE FROM " <> tableName @TickerInfo
      , "WHERE ticker = " <> show ticker -- must wrap in quotes
      ]
