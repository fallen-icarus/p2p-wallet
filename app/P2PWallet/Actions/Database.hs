module P2PWallet.Actions.Database
  ( -- * Creating the database.
    initializeDatabase

    -- * Profiles
  , loadProfiles
  , getNextProfileId
  , insertProfile
  , deleteProfile

    -- * Payment Wallets
  , loadPaymentWallets
  , getNextPaymentId
  , insertPaymentWallet
  , deletePaymentWallet

    -- * Transactions
  , loadTransactions
  , insertTransactions

    -- * Stake Wallets
  , loadStakeWallets
  , getNextStakeId
  , insertStakeWallet
  , deleteStakeWallet

    -- * Stake Rewards
  , loadRewards
  , insertRewards

    -- * Wallets
  , loadWallets

    -- * Address Book
  , loadAddressBook
  , getNextContactId
  , insertAddressEntry
  , deleteAddressEntry

    -- * Ticker Info
  , loadTickerInfo
  , insertTickerInfo
  , deleteTickerInfo
  ) where

import System.Directory qualified as Dir
import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.Core.StakeReward
import P2PWallet.Data.Core.Wallets
import P2PWallet.Database
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
-- | Load the profiles for the specified network. 
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

-- | Get the next profile id for a new profile.
getNextProfileId :: FilePath -> IO (Either Text ProfileId)
getNextProfileId dbFile =
    handle @SomeException (return . Left . ("Could not get next profile id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . ProfileId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT profile_id FROM"
      , tableName @Profile
      , "ORDER BY profile_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new profile to the database. This also updates profiles.
insertProfile :: FilePath -> Profile -> IO (Either Text ())
insertProfile dbFile profile = do
  handle @SomeException (return . Left . ("Failed to insert profile: " <>) . show) $
    Right <$> insert @Profile dbFile profile

-- | Delete the profile and all entries across the database that are for the target profile.
deleteProfile :: FilePath -> ProfileId -> IO (Either Text ())
deleteProfile dbFile (ProfileId profileId) = 
    handle @SomeException (return . Left . ("Failed to delete profile: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @Profile
        , tableName @PaymentWallet
        , tableName @Transaction
        , tableName @StakeWallet
        , tableName @StakeReward
        , tableName @AddressEntry
        -- The ticker registry is global (it applies to all profiles and networks).
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE profile_id = " <> show profileId
      ]

-------------------------------------------------
-- Payment Wallet
-------------------------------------------------
-- | Load the payment wallets for the specified profile.
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

-- | Get the next payment id for a new payment wallet.
getNextPaymentId :: FilePath -> IO (Either Text PaymentId)
getNextPaymentId dbFile =
    handle @SomeException (return . Left . ("Could not get next payment id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . PaymentId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT payment_id FROM"
      , tableName @PaymentWallet
      , "ORDER BY payment_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new payment wallet to the database. This also updates payment wallets.
insertPaymentWallet :: FilePath -> PaymentWallet -> IO (Either Text ())
insertPaymentWallet dbFile paymentWallet = do
  handle @SomeException (return . Left . ("Failed to insert payment wallet: " <>) . show) $
    Right <$> insert @PaymentWallet dbFile paymentWallet

-- | Delete a payment wallet and all of its entries across the database.
deletePaymentWallet :: FilePath -> PaymentId -> IO (Either Text ())
deletePaymentWallet dbFile (PaymentId paymentId) = 
    handle @SomeException (return . Left . ("Failed to delete payment wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @PaymentWallet
        , tableName @Transaction
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE payment_id = " <> show paymentId
      ]

-------------------------------------------------
-- Transactions
-------------------------------------------------
-- | Load the transactions for the specified payment wallet. They are loaded latest first.
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

-- | Add new transactions to the database.
insertTransactions :: FilePath -> [Transaction] -> IO (Either Text ())
insertTransactions dbFile txs = do
  handle @SomeException (return . Left . ("Failed to insert transactions: " <>) . show) $
    Right <$> insertMany @Transaction dbFile txs

-------------------------------------------------
-- Stake Wallet
-------------------------------------------------
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

-- | Add a new stake wallet to the database. This also updates stake wallets.
insertStakeWallet :: FilePath -> StakeWallet -> IO (Either Text ())
insertStakeWallet dbFile stakeWallet = do
  handle @SomeException (return . Left . ("Failed to insert stake wallet: " <>) . show) $
    Right <$> insert @StakeWallet dbFile stakeWallet

-- | Delete the specified stake wallet and all of its entries across the database.
deleteStakeWallet :: FilePath -> StakeId -> IO (Either Text ())
deleteStakeWallet dbFile (StakeId stakeId) = 
    handle @SomeException (return . Left . ("Failed to delete stake wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @StakeWallet
        , tableName @StakeReward
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE stake_id = " <> show stakeId
      ]

-------------------------------------------------
-- Stake Rewards
-------------------------------------------------
-- | Load the rewards for the specified stake wallet. They are loaded latest first.
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

-- | Add a new stake reward to the database.
insertRewards :: FilePath -> [StakeReward] -> IO (Either Text ())
insertRewards dbFile rs = do
  handle @SomeException (return . Left . ("Failed to insert rewards: " <>) . show) $
    Right <$> insertMany @StakeReward dbFile rs

-------------------------------------------------
-- Wallets
-------------------------------------------------
-- | Load the wallets for the specified profile.
loadWallets :: FilePath -> Profile -> IO (Either Text Wallets)
loadWallets dbFile Profile{..} = do
  handle @SomeException (return . Left . ("Could not load wallets: " <>) . show) $ do
    -- Load the payment wallets.
    paymentWalletsWithoutTxHistories <- loadPaymentWallets dbFile profileId >>= fromRightOrAppError

    -- Load the transaction histories for each payment wallet.
    paymentWalletsWithTxHistories <- forM paymentWalletsWithoutTxHistories $ 
      \paymentWallet -> do
        txs <- loadTransactions dbFile (paymentWallet ^. #paymentId) >>= fromRightOrAppError
        return $ paymentWallet & #transactions .~ txs

    -- Load the stake wallets.
    stakeWalletsWithoutRewardHistory <- loadStakeWallets dbFile profileId >>= fromRightOrAppError

    -- Load the reward histories for each stake wallet.
    stakeWalletsWithRewardHistory <- forM stakeWalletsWithoutRewardHistory $ 
      \stakeWallet -> do
        rewards <- loadRewards dbFile (stakeWallet ^. #stakeId) >>= fromRightOrAppError
        return $ stakeWallet & #rewardHistory .~ rewards

    return $ Right $ Wallets
      { paymentWallets = paymentWalletsWithTxHistories
      , stakeWallets = stakeWalletsWithRewardHistory
      }

-------------------------------------------------
-- Address Book
-------------------------------------------------
-- | Load the address book for the specified profile id. They are loaded alphabetically by contact
-- name.
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

getNextContactId :: FilePath -> IO (Either Text ContactId)
getNextContactId dbFile =
    handle @SomeException (return . Left . ("Could not get next contact id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . ContactId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT contact_id FROM"
      , tableName @AddressEntry
      , "ORDER BY contact_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new address entry to the database. This is also used to update an address entry.
insertAddressEntry :: FilePath -> [AddressEntry] -> IO (Either Text ())
insertAddressEntry dbFile entries = do
  handle @SomeException (return . Left . ("Failed to insert address entry: " <>) . show) $
    Right <$> insertMany @AddressEntry dbFile entries

-- | Delete a specific address entry.
deleteAddressEntry :: FilePath -> ContactId -> IO (Either Text ())
deleteAddressEntry dbFile (ContactId contactId) = 
  handle @SomeException (return . Left . ("Failed to delete address entry: " <>) . show) $ do
    Right <$> delete dbFile deleteSmt
  where
    deleteSmt :: Query
    deleteSmt = Query $ unwords
      [ "DELETE FROM " <> tableName @AddressEntry
      , "WHERE contact_id = " <> show contactId
      ]

-------------------------------------------------
-- Ticker Info
-------------------------------------------------
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

-- | Add a new ticker to the database. This is also used to update ticker info.
insertTickerInfo :: FilePath -> TickerInfo -> IO (Either Text ())
insertTickerInfo dbFile newTicker = do
  handle @SomeException (return . Left . ("Failed to insert ticker info: " <>) . show) $
    Right <$> insert @TickerInfo dbFile newTicker

-- | Delete a specific ticker info.
deleteTickerInfo :: FilePath -> Ticker -> IO (Either Text ())
deleteTickerInfo dbFile ticker = 
  handle @SomeException (return . Left . ("Failed to delete ticker info: " <>) . show) $ do
    Right <$> delete dbFile deleteSmt
  where
    deleteSmt :: Query
    deleteSmt = Query $ unwords
      [ "DELETE FROM " <> tableName @TickerInfo
      , "WHERE ticker = " <> show (display ticker) -- must wrap in quotes
      ]
