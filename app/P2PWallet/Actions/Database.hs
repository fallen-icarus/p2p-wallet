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
  , insertPaymentWallet
  , deletePaymentWallet
  , getNextPaymentWalletId

    -- * Stake Wallets
  , loadStakeWallets
  , getNextStakeWalletId
  , insertStakeWallet
  , deleteStakeWallet
  , changeDeFiWalletAliases

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

    -- * Dex Wallets
  , loadDexWallets
  , insertDexWallet
  , deleteDexWallet
  , getNextDexWalletId

    -- * Loan Wallets
  , loadLoanWallets
  , insertLoanWallet
  , deleteLoanWallet
  , getNextLoanWalletId

    -- * Options Wallets
  , loadOptionsWallets
  , insertOptionsWallet
  , deleteOptionsWallet
  , getNextOptionsWalletId

    -- * Aftermarket Wallets
  , loadAftermarketWallets
  , insertAftermarketWallet
  , deleteAftermarketWallet
  , getNextAftermarketWalletId
  ) where

import System.Directory qualified as Dir
import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Wallets
import P2PWallet.Database
import P2PWallet.Prelude

-------------------------------------------------
-- Create a new database
-------------------------------------------------
initializeDatabase :: FilePath -> IO (Either Text ())
initializeDatabase dbFile = do
  handle @SomeException (return . Left . ("Could not initialize database: " <>) . show) $ do
    -- Create the tables in the database if they don't already exist.
    create @Profile dbFile
    create @PaymentWallet dbFile
    create @StakeWallet dbFile
    create @AddressEntry dbFile
    create @TickerInfo dbFile
    create @DexWallet dbFile
    create @LoanWallet dbFile
    create @OptionsWallet dbFile
    create @MarketWallet dbFile
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
        , tableName @StakeWallet
        , tableName @AddressEntry
        , tableName @DexWallet
        , tableName @LoanWallet
        , tableName @OptionsWallet
        , tableName @MarketWallet
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
    handle @SomeException (return . Left . ("Could not load payment wallets: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @PaymentWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY payment_wallet_id ASC;"
      ]

-- | Add a new payment wallet to the database. This also updates payment wallets.
insertPaymentWallet :: FilePath -> PaymentWallet -> IO (Either Text ())
insertPaymentWallet dbFile paymentWallet = do
  handle @SomeException (return . Left . ("Failed to insert payment wallet: " <>) . show) $
    Right <$> insert @PaymentWallet dbFile paymentWallet

-- | Delete a payment wallet and all of its entries across the database.
deletePaymentWallet :: FilePath -> PaymentWalletId -> IO (Either Text ())
deletePaymentWallet dbFile (PaymentWalletId paymentWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete payment wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @PaymentWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE payment_wallet_id = " <> show paymentWalletId
      ]

getNextPaymentWalletId :: FilePath -> IO (Either Text PaymentWalletId)
getNextPaymentWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next payment wallet id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . PaymentWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT payment_wallet_id FROM"
      , tableName @PaymentWallet
      , "ORDER BY payment_wallet_id DESC"
      , "LIMIT 1;"
      ]

-------------------------------------------------
-- Stake Wallet
-------------------------------------------------
-- | Load the stake wallets for the specified profile
loadStakeWallets :: FilePath -> ProfileId -> IO (Either Text [StakeWallet])
loadStakeWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load stake wallets: " <>) . show) $ do
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @StakeWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY stake_wallet_id ASC;"
      ]

getNextStakeWalletId :: FilePath -> IO (Either Text StakeWalletId)
getNextStakeWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next stake id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . StakeWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT stake_wallet_id FROM"
      , tableName @StakeWallet
      , "ORDER BY stake_wallet_id DESC"
      , "LIMIT 1;"
      ]

-- | Add a new stake wallet to the database. This also updates stake wallets.
insertStakeWallet :: FilePath -> StakeWallet -> IO (Either Text ())
insertStakeWallet dbFile stakeWallet = do
  handle @SomeException (return . Left . ("Failed to insert stake wallet: " <>) . show) $
    Right <$> insert @StakeWallet dbFile stakeWallet

-- | Delete the specified stake wallet and all of its entries across the database.
deleteStakeWallet :: FilePath -> StakeWalletId -> IO (Either Text ())
deleteStakeWallet dbFile (StakeWalletId stakeWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete stake wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @StakeWallet
        , tableName @DexWallet
        , tableName @LoanWallet
        , tableName @OptionsWallet
        , tableName @MarketWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE stake_wallet_id = " <> show stakeWalletId
      ]

-- | Change all aliases for the defi wallets using the specified stake credential. The aliases
-- need to stay synced.
changeDeFiWalletAliases :: FilePath -> StakeWalletId -> Text -> IO (Either Text ())
changeDeFiWalletAliases dbFile (StakeWalletId stakeWalletId) newAlias =
    handle @SomeException (return . Left . ("Failed to update defi aliases: " <>) . show) $ do
      Right <$> mapM_ (update dbFile . updateStmt)
        [ tableName @DexWallet
        , tableName @LoanWallet
        , tableName @OptionsWallet
        , tableName @MarketWallet
        ]
  where
    updateStmt :: Text -> Query
    updateStmt table = Query $ unwords
      [ "UPDATE " <> table
      , "SET alias = " <> show newAlias
      , "WHERE stake_wallet_id = " <> show stakeWalletId
      ]

-------------------------------------------------
-- Wallets
-------------------------------------------------
-- | Load the wallets for the specified profile.
loadWallets :: FilePath -> Profile -> IO (Either Text Wallets)
loadWallets dbFile Profile{..} = do
  handle @SomeException (return . Left . ("Could not load wallets: " <>) . show) $ do
    -- Load the payment wallets.
    paymentWallets <- loadPaymentWallets dbFile profileId >>= fromRightOrAppError

    -- Load the stake wallets.
    stakeWallets <- loadStakeWallets dbFile profileId >>= fromRightOrAppError

    -- Load the dex wallets.
    dexWallets <- loadDexWallets dbFile profileId >>= fromRightOrAppError

    -- Load the loan wallets.
    loanWallets <- loadLoanWallets dbFile profileId >>= fromRightOrAppError

    -- Load the options wallets.
    optionsWallets <- loadOptionsWallets dbFile profileId >>= fromRightOrAppError

    -- Load the aftermarket wallets.
    aftermarketWallets <- loadAftermarketWallets dbFile profileId >>= fromRightOrAppError

    return $ Right $ Wallets
      { paymentWallets = paymentWallets
      , stakeWallets = stakeWallets
      , dexWallets = dexWallets
      , loanWallets = loanWallets
      , optionsWallets = optionsWallets
      , marketWallets = aftermarketWallets
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

-------------------------------------------------
-- Dex Wallet
-------------------------------------------------
-- | Load the dex wallets for the specified profile.
loadDexWallets :: FilePath -> ProfileId -> IO (Either Text [DexWallet])
loadDexWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load dex wallets: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @DexWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY dex_wallet_id ASC;"
      ]

-- | Add a new dex wallet to the database. This also updates dex wallets.
insertDexWallet :: FilePath -> DexWallet -> IO (Either Text ())
insertDexWallet dbFile dexWallet = do
  handle @SomeException (return . Left . ("Failed to insert dex wallet: " <>) . show) $
    Right <$> insert @DexWallet dbFile dexWallet

-- | Delete a dex wallet and all of its entries across the database.
deleteDexWallet :: FilePath -> DexWalletId -> IO (Either Text ())
deleteDexWallet dbFile (DexWalletId dexWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete dex wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @DexWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE dex_wallet_id = " <> show dexWalletId
      ]

getNextDexWalletId :: FilePath -> IO (Either Text DexWalletId)
getNextDexWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next dex wallet id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . DexWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT dex_wallet_id FROM"
      , tableName @DexWallet
      , "ORDER BY dex_wallet_id DESC"
      , "LIMIT 1;"
      ]

-------------------------------------------------
-- Loan Wallet
-------------------------------------------------
-- | Load the loan wallets for the specified profile.
loadLoanWallets :: FilePath -> ProfileId -> IO (Either Text [LoanWallet])
loadLoanWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load loan wallets: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @LoanWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY loan_wallet_id ASC;"
      ]

-- | Add a new loan wallet to the database. This also updates loan wallets.
insertLoanWallet :: FilePath -> LoanWallet -> IO (Either Text ())
insertLoanWallet dbFile loanWallet = do
  handle @SomeException (return . Left . ("Failed to insert loan wallet: " <>) . show) $
    Right <$> insert @LoanWallet dbFile loanWallet

-- | Delete a loan wallet and all of its entries across the database.
deleteLoanWallet :: FilePath -> LoanWalletId -> IO (Either Text ())
deleteLoanWallet dbFile (LoanWalletId loanWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete loan wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @LoanWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE loan_wallet_id = " <> show loanWalletId
      ]

getNextLoanWalletId :: FilePath -> IO (Either Text LoanWalletId)
getNextLoanWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next loan wallet id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . LoanWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT loan_wallet_id FROM"
      , tableName @LoanWallet
      , "ORDER BY loan_wallet_id DESC"
      , "LIMIT 1;"
      ]

-------------------------------------------------
-- Options Wallet
-------------------------------------------------
-- | Load the options wallets for the specified profile.
loadOptionsWallets :: FilePath -> ProfileId -> IO (Either Text [OptionsWallet])
loadOptionsWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load options wallets: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @OptionsWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY options_wallet_id ASC;"
      ]

-- | Add a new options wallet to the database. This also updates options wallets.
insertOptionsWallet :: FilePath -> OptionsWallet -> IO (Either Text ())
insertOptionsWallet dbFile optionsWallet = do
  handle @SomeException (return . Left . ("Failed to insert options wallet: " <>) . show) $
    Right <$> insert @OptionsWallet dbFile optionsWallet

-- | Delete a options wallet and all of its entries across the database.
deleteOptionsWallet :: FilePath -> OptionsWalletId -> IO (Either Text ())
deleteOptionsWallet dbFile (OptionsWalletId optionsWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete options wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @OptionsWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE options_wallet_id = " <> show optionsWalletId
      ]

getNextOptionsWalletId :: FilePath -> IO (Either Text OptionsWalletId)
getNextOptionsWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next options wallet id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . OptionsWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT options_wallet_id FROM"
      , tableName @OptionsWallet
      , "ORDER BY options_wallet_id DESC"
      , "LIMIT 1;"
      ]

-------------------------------------------------
-- Aftermarket Wallet
-------------------------------------------------
-- | Load the aftermarket wallets for the specified profile.
loadAftermarketWallets :: FilePath -> ProfileId -> IO (Either Text [MarketWallet])
loadAftermarketWallets dbFile (ProfileId profileId) = do
    handle @SomeException (return . Left . ("Could not load aftermarket wallets: " <>) . show) $
      Right <$> query dbFile queryStmt
  where
    queryStmt :: Query
    queryStmt = Query $ unwords
      [ "SELECT * FROM " <> tableName @MarketWallet
      , "WHERE profile_id = " <> show profileId
      , "ORDER BY market_wallet_id ASC;"
      ]

-- | Add a new market wallet to the database. This also updates market wallets.
insertAftermarketWallet :: FilePath -> MarketWallet -> IO (Either Text ())
insertAftermarketWallet dbFile marketWallet = do
  handle @SomeException (return . Left . ("Failed to insert aftermarket wallet: " <>) . show) $
    Right <$> insert @MarketWallet dbFile marketWallet

-- | Delete an aftermarket wallet and all of its entries across the database.
deleteAftermarketWallet :: FilePath -> MarketWalletId -> IO (Either Text ())
deleteAftermarketWallet dbFile (MarketWalletId marketWalletId) = 
    handle @SomeException (return . Left . ("Failed to delete aftermarket wallet: " <>) . show) $ do
      Right <$> mapM_ (delete dbFile . deleteStmt)
        [ tableName @MarketWallet
        ]
  where
    deleteStmt :: Text -> Query
    deleteStmt table = Query $ unwords
      [ "DELETE FROM " <> table
      , "WHERE market_wallet_id = " <> show marketWalletId
      ]

getNextAftermarketWalletId :: FilePath -> IO (Either Text MarketWalletId)
getNextAftermarketWalletId dbFile =
    handle @SomeException (return . Left . ("Could not get next aftermarket wallet id: " <>) . show) $
      -- If the result is the empty list, this is the first entry.
      maybe (Right 0) (Right . MarketWalletId . (+1) . Sqlite.fromOnly) . maybeHead <$> 
        query dbFile stmt
  where
    stmt = Sqlite.Query $ mconcat $ intersperse " "
      [ "SELECT market_wallet_id FROM"
      , tableName @MarketWallet
      , "ORDER BY market_wallet_id DESC"
      , "LIMIT 1;"
      ]
