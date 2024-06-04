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
  ) where

import System.Directory qualified as Dir
import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Actions.Utils
import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Data.Profile
import P2PWallet.Data.Transaction
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
      create @Transaction dbFile
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

    return $ Right $ Wallets
      { paymentWallets = paymentWalletsWithTxHistories
      }

-- | Load the payment wallets for the specified profile
loadPaymentWallets :: FilePath -> ProfileId -> IO (Either Text [PaymentWallet])
loadPaymentWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load payment wallets : " <>) . show) $ do
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
    fmap sequence_ $ mapM (fmap Right . insert @Transaction dbFile) txs

-- | Load the transactions for the specified payment wallet.
loadTransactions :: FilePath -> PaymentId -> IO (Either Text [Transaction])
loadTransactions dbFile (PaymentId paymentId) = do
    handle @SomeException (return . Left . ("Could not load transactions: " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @Transaction
      , "WHERE payment_id = " <> show paymentId
      , "ORDER BY block_height DESC;"
      ]
