{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module P2PWallet.Actions.Query.Koios
  (
    runSubmitTx
  , runEvaluateTx
  , runGetParams
  , runQueryPaymentWalletInfo
  , runQueryStakeWalletInfo
  , runQueryAllRegisteredPools
  , runQuerySwaps
  , runQueryDexWallet
  , runQueryLoanWallet
  , runQueryLoanAsks
  , runQueryLoanOffers
  , runQueryActiveLoans
  , runQuerySpecificLoan
  , runQueryBorrowerCreditHistory
  , runQueryBorrowerInformation
  , runQueryOptionsWallet
  ) where

import Servant.Client (client , ClientM , runClientM , Scheme(Https) , BaseUrl(..) , mkClientEnv)
import Servant.Client qualified as Client
import Servant.API 
  ( ToHttpApiData
  , (:<|>)(..)
  , JSON
  , Post
  , Get
  , (:>)
  , ReqBody
  , Required
  , QueryParam'
  , QueryParam
  )
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.HTTP.Client (newManager)
import Data.Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import UnliftIO.Async (mapConcurrently,concurrently)

import P2PWallet.Data.AppModel.LendingModel.ActiveLoanConfiguration
import P2PWallet.Data.AppModel.LendingModel.LoanAskConfiguration
import P2PWallet.Data.AppModel.LendingModel.LoanOfferConfiguration
import P2PWallet.Data.Core.BorrowerInformation
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.AssetTransaction
import P2PWallet.Data.Koios.LinkedPaymentAddresses
import P2PWallet.Data.Koios.MintTransaction
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Koios.PostTypes
import P2PWallet.Data.Koios.StakeAccount
import P2PWallet.Data.Koios.StakeReward
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Core.Transaction qualified as P2P
import P2PWallet.Data.Core.StakeReward qualified as P2P
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toNetworkURL :: Network -> String
toNetworkURL Mainnet = "api.koios.rest"
toNetworkURL Testnet = "preprod.koios.rest"

-- | Koios occassionally takes too long to respond. When this happens, the query should just
-- be retried.
handleTimeoutError :: IO (Either Client.ClientError a) -> IO (Either Client.ClientError a)
handleTimeoutError query = query >>= \case
    Left err -> if isTimeoutError err then handleTimeoutError query else return $ Left err
    Right res -> return $ Right res
  where
    isTimeoutError :: Client.ClientError -> Bool
    isTimeoutError err =
      "ResponseTimeout)" == Text.takeEnd 16 (show err)

-- | Koios instances occasionally take too long to respond. If they take longer than 5 seconds,
-- odds are, they won't reply by 30 seconds either (the default timeout). These settings just change
-- the default timeout to 5 seconds.
customTlsSettings :: HTTP.ManagerSettings
customTlsSettings = HTTPS.tlsManagerSettings
  { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5_000_000
  }

-- Since Koios instances may occassionally return incorrect UTxOs if they are not
-- close enough to the chain tip, this would mess with the wallet's notifications.
-- To account for this, the UTxOs are queried three times and compared. At least
-- two responses must match to move on. The redundant queries occur concurrently.
queryWithRedundancies :: (Eq a) => IO (Either Text [a]) -> IO (Either Text [a])
queryWithRedundancies query = do
  (res1,(res2,res3)) <- concurrently query (concurrently query query)
  let path1 = liftA3 (\a b c -> a == b || a == c) res1 res2 res3
      path2 = liftA2 (==) res2 res3
  case (path1,path2) of
    (Right True ,_) -> return $ first show res1
    (_, Right True) -> return $ first show res2
    (Left err, _) -> return $ Left $ show err
    (_, Left err) -> return $ Left $ show err
    _ -> return $ Left "There was an error syncing. Wait a minute and try again."

-------------------------------------------------
-- High-Level API
-------------------------------------------------
-- | Submit a transaction through Koios.
runSubmitTx :: Network -> FilePath -> IO (Either Text Value)
runSubmitTx network txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file."
    Just tx -> do
      manager <- newManager customTlsSettings
      res <-
        handleTimeoutError $ runClientM (submitApi $ SubmitTxCBOR tx) $
          mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

-- | Estimate the execution budgets for the transaction using Koios.
runEvaluateTx :: Network -> FilePath -> IO (Either Text Value)
runEvaluateTx network txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file"
    Just tx -> do
      manager <- newManager customTlsSettings
      res <-
        handleTimeoutError $ runClientM (evaluateApi $ EvaluateTxCBOR tx) $
          mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

-- | Get the current parameters for use with cardano-cli. Also extract out the collateral percentage
-- which is required for determining how much collateral to take from a collateral uxo.
runGetParams :: Network -> IO (Either Text (ByteString,Decimal))
runGetParams network = do
    manager' <- newManager customTlsSettings
    let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network) 443 "api/v1")
    handleTimeoutError (runClientM paramsApi env) >>= 
      either (return . Left . show) (return . processResults)
  where
    processResults :: Value -> Either Text (ByteString,Decimal)
    processResults val = (valueAsByteString val,) <$> parseCollateralPercentage val

    parseCollateralPercentage :: Value -> Either Text Decimal
    parseCollateralPercentage (Object o) = 
      maybe (Left "Could not parse Koios parameters.") (Right . (/100) . fromIntegral @_ @Decimal) $
        Aeson.parseMaybe (\x -> Aeson.parseField @Integer x "collateralPercentage") o
    parseCollateralPercentage val = 
      Left $ "Could not parse Koios parameters: " <> showValue val

-- | Sync the latest information for the payment wallet. Try to do as much concurrently as possible.
-- There are redundant queries built-in since Koios can occassionally return incorrect information
-- if a particular instance is not properly synced/configured.
runQueryPaymentWalletInfo :: PaymentWallet -> IO (Either Text PaymentWallet)
runQueryPaymentWalletInfo wallet@PaymentWallet{..} = do
    (utxoRes,txRes) <- concurrently queryUTxOsWithRedundancies queryTxsConcurrently

    case (,) <$> utxoRes <*> txRes of
      Right (us,txs) -> do
        return $ Right $ 
          wallet & #utxos .~ map fromAddressUTxO us
                 & #lovelace .~ sum (map (view #lovelace) us)
                 & #transactions %~ mappend (map P2P.toTransaction txs)
                 & populateNativeAssets
      Left err -> return $ Left err
  where
    -- | Aggregate all native assets located at the address.
    populateNativeAssets :: PaymentWallet -> PaymentWallet
    populateNativeAssets p@PaymentWallet{utxos=us} =
      p & #nativeAssets .~ sumNativeAssets (concatMap (view #nativeAssets) us)

    -- Add one to the blockHeight for the most recently recorded transaction for this wallet.
    -- Koios will return any transactions for that block or later.
    afterBlock :: Integer
    afterBlock = (+1) $ maybe 0 (view #blockHeight) $ maybeHead transactions

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryUTxOsWithRedundancies :: IO (Either Text [AddressUTxO])
    queryUTxOsWithRedundancies = queryWithRedundancies fetchUTxOs

    -- Try to query the UTxOs. If a timeout error occurs, just try again.
    fetchUTxOs :: IO (Either Text [AddressUTxO])
    fetchUTxOs = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [paymentAddress]) env)

    -- Try to query the tx hashes since last time. If a timeout error occurs, just try again.
    fetchTxHashes :: IO (Either Text [Text])
    fetchTxHashes = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryAddressTxHashes [paymentAddress] afterBlock) env)

    -- Try to query the transaction info concurrently. The transactions are grouped together,
    -- 50 per response, since some transactions can be quite large.
    queryTxsConcurrently :: IO (Either Text [Transaction])
    queryTxsConcurrently = do
      fetchTxHashes >>= \case
        Left err -> return $ Left $ show err
        Right hashes -> do
          manager <- newManager customTlsSettings
          let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          bimap show concat . sequence <$> 
            mapConcurrently 
              (\hs -> handleTimeoutError $ runClientM (queryAddressTransactions hs) env)
              (groupInto 70 hashes)

-- | Sync the latest information for the stake wallet. Try to do as much concurrently as possible.
runQueryStakeWalletInfo :: StakeWallet -> IO (Either Text StakeWallet)
runQueryStakeWalletInfo wallet@StakeWallet{network,stakeAddress} = do
    (res1,(res2,res3)) <- 
      concurrently fetchAccountStatus $ concurrently fetchRewards fetchLinkedAddresses
    case (,,) <$> res1 <*> res2 <*> res3 of
      Right ([StakeAccount{stakeAddress=_,..}],rewards,linkedAddresses) ->
        -- This query relies on the response from the `fetchAccountStatus` query.
        fetchDelegatedPoolInfo delegatedPool >>= \case
          Left err -> return $ Left $ show err
          Right pools -> do
            return $ Right $
              wallet & #registrationStatus .~ registrationStatus
                     & #totalDelegation .~ totalDelegation
                     & #utxoBalance .~ utxoBalance
                     & #availableRewards .~ availableRewards
                     & #delegatedPool .~ maybeHead pools
                     & #rewardHistory .~ reverse (map P2P.toStakeReward rewards)
                     & #linkedAddresses .~ linkedAddresses
      Right ([],_,_) -> 
        -- If a stake address has never been seen on chain before (ie, a UTxO has not been created 
        -- at a payment address using the staking credential), the query will return the empty list.
        -- The preset fields for the account are accurate in this scenario.
        return $ Right wallet
      Right _ -> do
        return $ Left "Stake account query returned an unexpected number of arguments."
      Left err -> return $ Left $ show err
  where
    -- Try to query the stake address' status.
    fetchAccountStatus :: IO (Either Text [StakeAccount])
    fetchAccountStatus = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryStakeAccounts [stakeAddress]) env)

    -- Try to query the stake address' rewards.
    fetchRewards :: IO (Either Text [StakeReward])
    fetchRewards = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryStakeRewards [stakeAddress]) env)
    
    -- Try to query the information for the pool this stake address is delegated to.
    fetchDelegatedPoolInfo :: Maybe PoolID -> IO (Either Text [Pool])
    fetchDelegatedPoolInfo mDelegatedPool = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          query = queryPoolInfo Nothing Nothing $ Pools $ catMaybes [mDelegatedPool]
      first show <$> handleTimeoutError (runClientM query env)
    
    -- Try to query the stake address' rewards.
    fetchLinkedAddresses :: IO (Either Text [PaymentAddress])
    fetchLinkedAddresses = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryLinkedPaymentAddresses [stakeAddress]) env)
    
-- | Get all registered pools for the given network. This tries to do as much concurrently as
-- possible.
runQueryAllRegisteredPools :: Network -> IO (Either Text [Pool])
runQueryAllRegisteredPools network = do
    manager' <- newManager customTlsSettings
    let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network) 443 "api/v1")

    -- Get the list of pool ids. Group them 70 at a time so that the next query is not too large.
    poolIds <- handleTimeoutError $ fmap (groupInto 70) <$> runClientM (queryPoolIds 0 []) env

    -- If the previous query returned an error, just return the error. Otherwise, try to get
    -- the information for each pool.
    info <- 
      flip (either (return . Left)) poolIds $ 
        fmap sequence . mapConcurrently
          (\ids -> handleTimeoutError $ 
            runClientM (queryPoolInfo (Just "not.is.null") (Just "not.is.null") $ Pools ids) env)

    case info of
      Right rs -> return $ Right $ concat rs
      Left err -> return $ Left $ show err

-- | Get the current order-book for a specific trading pair. This queries both one-way and two-way
-- swaps. Ask and offer queries are _not_ supported.
runQuerySwaps :: Network -> OfferAsset -> AskAsset -> IO (Either Text [SwapUTxO])
runQuerySwaps network offerAsset askAsset = do
    (oneWayRes,twoWayRes) <- concurrently fetchOneWaySwaps fetchTwoWaySwaps

    case (,) <$> oneWayRes <*> twoWayRes of
      Right (oneWayUTxOs,twoWayUTxOs) -> do
        return $ Right $ sortOn (swapUTxOPrice offerAsset askAsset) $ 
          map fromAddressUTxO $ oneWayUTxOs <> twoWayUTxOs
      Left err -> return $ Left err
  where
    -- Try to query the one-way swaps.
    fetchOneWaySwaps :: IO (Either Text [AddressUTxO])
    fetchOneWaySwaps = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryOneWaySwaps offerAsset askAsset) env)

    -- Try to query the two-way swaps.
    fetchTwoWaySwaps :: IO (Either Text [AddressUTxO])
    fetchTwoWaySwaps = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryTwoWaySwaps offerAsset askAsset) env)

-- | Get all information for a particular dex wallet.
runQueryDexWallet :: DexWallet -> IO (Either Text DexWallet)
runQueryDexWallet dexWallet@DexWallet{..} = do
    (txRes,(oneWayRes,twoWayRes)) <- 
      concurrently queryTxsConcurrently $
        concurrently queryOneWaySwapsWithRedundancies queryTwoWaySwapsWithRedundancies

    case (,,) <$> txRes <*> oneWayRes <*> twoWayRes of
      Right (txs,oneWayUTxOs,twoWayUTxOs) -> do
        let allUTxOs = oneWayUTxOs <> twoWayUTxOs
        return $ Right $ dexWallet
          & #lovelace .~ sum (map (view #lovelace) allUTxOs)
          & #utxos .~ map fromAddressUTxO allUTxOs
          & populateNativeAssets
          & #transactions %~ mappend (map P2P.toTransaction txs)
      Left err -> return $ Left err
  where
    -- | Aggregate all native assets located at the wallet.
    populateNativeAssets :: DexWallet -> DexWallet
    populateNativeAssets s@DexWallet{utxos=us} =
      s & #nativeAssets .~ sumNativeAssets (concatMap (view #nativeAssets) us)

    -- Add one to the blockHeight for the most recently recorded transaction for this wallet.
    -- Koios will return any transactions for that block or later.
    afterBlock :: Integer
    afterBlock = (+1) $ maybe 0 (view #blockHeight) $ maybeHead transactions

    -- Try to query the one-way swaps.
    fetchOneWaySwaps :: IO (Either Text [AddressUTxO])
    fetchOneWaySwaps = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [oneWaySwapAddress]) env)

    -- Try to query the two-way swaps.
    fetchTwoWaySwaps :: IO (Either Text [AddressUTxO])
    fetchTwoWaySwaps = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [twoWaySwapAddress]) env)

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryOneWaySwapsWithRedundancies :: IO (Either Text [AddressUTxO])
    queryOneWaySwapsWithRedundancies = queryWithRedundancies fetchOneWaySwaps

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryTwoWaySwapsWithRedundancies :: IO (Either Text [AddressUTxO])
    queryTwoWaySwapsWithRedundancies = queryWithRedundancies fetchTwoWaySwaps

    -- Try to query the tx hashes since last time. If a timeout error occurs, just try again.
    fetchTxHashes :: IO (Either Text [Text])
    fetchTxHashes = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      -- Since this is querying the addresses for both swap addresses, it is possible for a
      -- transaction to be for both of them. These duplicates should be removed.
      bimap show ordNub <$> 
        handleTimeoutError 
          (runClientM (queryAddressTxHashes [oneWaySwapAddress,twoWaySwapAddress] afterBlock) env)

    -- Try to query the transaction info concurrently. The transactions are grouped together,
    -- 50 per response, since some transactions can be quite large.
    queryTxsConcurrently :: IO (Either Text [Transaction])
    queryTxsConcurrently = do
      fetchTxHashes >>= \case
        Left err -> return $ Left $ show err
        Right hashes -> do
          manager <- newManager customTlsSettings
          let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          bimap show concat . sequence <$> 
            mapConcurrently 
              (\hs -> handleTimeoutError $ runClientM (queryAddressTransactions hs) env)
              (groupInto 70 hashes)

-- | Get all information for a particular loan wallet.
runQueryLoanWallet :: LoanWallet -> IO (Either Text LoanWallet)
runQueryLoanWallet loanWallet@LoanWallet{..} = do
    (txRes,(loansRes,offerRes)) <- 
      concurrently queryTxsConcurrently $ 
        concurrently queryLoansWithRedundancies queryOffersWithRedundancies

    (creditHistoryRes, offerTxsRes) <- 
      concurrently 
        (runQueryBorrowerCreditHistory network $ Loans.genBorrowerId stakeCredential)
        queryOfferTxsConcurrently

    case (,,,,) <$> txRes <*> loansRes <*> offerRes <*> creditHistoryRes <*> offerTxsRes of
      Right (txs,loanUTxOs,offers,history, offerTxs) -> do
        return $ Right $ loanWallet
          & #lovelace .~ sum (map (view #lovelace) loanUTxOs)
          & #utxos .~ map fromAddressUTxO loanUTxOs
          & populateNativeAssets
          & #transactions %~ mappend (map P2P.toTransaction txs)
          & #offerUTxOs .~ map fromAddressUTxO offers
          & #creditHistory .~ history
          & #offerTransactions %~ mappend (map P2P.toTransaction offerTxs)
      Left err -> return $ Left err
  where
    -- | Aggregate all native assets located at the wallet.
    populateNativeAssets :: LoanWallet -> LoanWallet
    populateNativeAssets s@LoanWallet{utxos=us} =
      s & #nativeAssets .~ sumNativeAssets (concatMap (view #nativeAssets) us)

    -- Add one to the blockHeight for the most recently recorded transaction for this wallet.
    -- Koios will return any transactions for that block or later.
    afterBlock :: Integer
    afterBlock = (+1) $ maybe 0 (view #blockHeight) $ maybeHead transactions

    -- Add one to the blockHeight for the most recently recorded transaction for this wallet.
    -- Koios will return any transactions for that block or later.
    afterOfferBlock :: Maybe Integer
    afterOfferBlock = (+1) . view #blockHeight <$> maybeHead offerTransactions

    -- Try to query the loan address' utxos.
    fetchLoanUTxOs :: IO (Either Text [AddressUTxO])
    fetchLoanUTxOs = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [loanAddress]) env)

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryLoansWithRedundancies :: IO (Either Text [AddressUTxO])
    queryLoansWithRedundancies = queryWithRedundancies fetchLoanUTxOs

    -- Try to query the offer utxos tied to this user's credential.
    fetchOfferUTxOs :: IO (Either Text [AddressUTxO])
    fetchOfferUTxOs = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          offerCfg = def & #lenderCredential ?~ stakeCredential
      first show <$> handleTimeoutError (runClientM (queryLoanOffers offerCfg) env)

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryOffersWithRedundancies :: IO (Either Text [AddressUTxO])
    queryOffersWithRedundancies = queryWithRedundancies fetchOfferUTxOs

    -- Try to query the tx hashes since last time. If a timeout error occurs, just try again.
    fetchTxHashes :: IO (Either Text [Text])
    fetchTxHashes = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryAddressTxHashes [loanAddress] afterBlock) env)

    -- Try to query the transaction info concurrently. The transactions are grouped together,
    -- 50 per response, since some transactions can be quite large.
    queryTxsConcurrently :: IO (Either Text [Transaction])
    queryTxsConcurrently = do
      fetchTxHashes >>= \case
        Left err -> return $ Left $ show err
        Right hashes -> do
          manager <- newManager customTlsSettings
          let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          bimap show concat . sequence <$> 
            mapConcurrently 
              (\hs -> handleTimeoutError $ runClientM (queryAddressTransactions hs) env)
              (groupInto 70 hashes)

    -- Try to query the offer tx hashes since last time. If a timeout error occurs, just try 
    -- again.
    fetchOfferTxHashes :: IO (Either Text [Text])
    fetchOfferTxHashes = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          lenderId = Loans.unLenderId $ Loans.genLenderId stakeCredential
          negotiationId = Loans.negotiationBeaconCurrencySymbol
      first show <$> 
        handleTimeoutError (runClientM (queryAssetTxHashes negotiationId lenderId afterOfferBlock) env)

    -- Try to query the transaction info concurrently. The transactions are grouped together,
    -- 50 per response, since some transactions can be quite large.
    queryOfferTxsConcurrently :: IO (Either Text [Transaction])
    queryOfferTxsConcurrently = do
      fetchOfferTxHashes >>= \case
        Left err -> return $ Left $ show err
        Right hashes -> do
          manager <- newManager customTlsSettings
          let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          bimap show concat . sequence <$> 
            mapConcurrently 
              (\hs -> handleTimeoutError $ runClientM (queryAddressTransactions hs) env)
              (groupInto 70 hashes)

-- | Get the current open loan asks. Optionally filter the asks by loan asset and collateral.
runQueryLoanAsks :: Network -> LoanAskConfiguration -> IO (Either Text [LoanUTxO])
runQueryLoanAsks network loanAskCfg = do
    askRes <- fetchOpenAsks

    case askRes of
      Right openAsks -> do
        return $ Right $ sortOn loanUTxOLoanAmount $ map fromAddressUTxO openAsks
      Left err -> return $ Left err
  where
    -- Try to query the open asks.
    fetchOpenAsks :: IO (Either Text [AddressUTxO])
    fetchOpenAsks = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryLoanAsks loanAskCfg) env)

-- | Query the event history for a particular loan.
runQuerySpecificLoan 
  :: Network 
  -> Loans.LoanId 
  -> IO (Either Text ([LoanEvent], Maybe LoanUTxO))
runQuerySpecificLoan network loanId = do
    (historyRes, stateRes) <- concurrently fetchEventHistory
      (fmap (fmap fromAddressUTxO . maybeHead) <$> fetchLoanState)

    return $ (,) <$> historyRes <*> stateRes
  where
    -- Try to query the loan event history.
    fetchEventHistory :: IO (Either Text [LoanEvent])
    fetchEventHistory = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryLoanTxHistory loanId) env)

    -- Try to query the current loan state.
    fetchLoanState :: IO (Either Text [AddressUTxO])
    fetchLoanState = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (querySpecificLoanUTxO loanId) env)

-- | Query the credit history for a particular borrower.
runQueryBorrowerCreditHistory :: Network -> Loans.BorrowerId -> IO (Either Text [LoanResult])
runQueryBorrowerCreditHistory network borrowerId = do
  manager <- newManager customTlsSettings
  let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
  first show <$> handleTimeoutError (runClientM (queryBorrowerCreditHistory borrowerId) env)

runQueryBorrowerInformation 
  :: Network 
  -> Loans.BorrowerId 
  -> PaymentAddress
  -> IO (Either Text BorrowerInformation)
runQueryBorrowerInformation network borrowerId borrowerAddress = do
    (addrRes, historyRes) <- 
      concurrently queryLoansWithRedundancies (runQueryBorrowerCreditHistory network borrowerId)

    case (,) <$> addrRes <*> historyRes of
      Right (addrUTxOs,history) -> do
        let loanUTxOs = map fromAddressUTxO addrUTxOs
            hasBeacon targetPolicy targetName = 
              any $ \NativeAsset{policyId,tokenName} -> 
                policyId == targetPolicy && tokenName == targetName
            beaconCheck targetPolicy targetName = 
              hasBeacon targetPolicy targetName . view #nativeAssets
        return $ Right $ BorrowerInformation
          { borrowerId = borrowerId
          , loanAddress = borrowerAddress
          , openAsks = 
              filter (beaconCheck Loans.negotiationBeaconCurrencySymbol Loans.askBeaconName) $ 
                filter (isJust . preview (#loanDatum % _Just % _AskDatum)) loanUTxOs
          , currentOffers = 
              filter (beaconCheck Loans.negotiationBeaconCurrencySymbol Loans.offerBeaconName) $ 
                filter (isJust . preview (#loanDatum % _Just % _OfferDatum)) loanUTxOs
          , activeLoans = 
              filter (beaconCheck Loans.activeBeaconCurrencySymbol Loans.activeBeaconName) $ 
                filter (isJust . preview (#loanDatum % _Just % _ActiveDatum)) loanUTxOs
          , creditHistory = history
          , showCreditHistory = False
          , showOpenAsks = False
          , showCurrentOffers = False
          , showActiveLoans = False
          }
      Left err -> return $ Left err
  where
    -- Try to query the loan address' utxos.
    fetchLoanUTxOs :: IO (Either Text [AddressUTxO])
    fetchLoanUTxOs = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [borrowerAddress]) env)

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryLoansWithRedundancies :: IO (Either Text [AddressUTxO])
    queryLoansWithRedundancies = queryWithRedundancies fetchLoanUTxOs

-- | Get the current loan offers that match the desired configuration.
runQueryLoanOffers :: Network -> LoanOfferConfiguration -> IO (Either Text [LoanUTxO])
runQueryLoanOffers network loanOfferCfg = do
    offerRes <- fetchOffers

    case offerRes of
      Right offers -> do
        return $ Right $ sortOn loanUTxOLoanAmount $ map fromAddressUTxO offers
      Left err -> return $ Left err
  where
    -- Try to query the offers.
    fetchOffers :: IO (Either Text [AddressUTxO])
    fetchOffers = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryLoanOffers loanOfferCfg) env)

-- | Get the current active loans that match the desired configuration.
runQueryActiveLoans :: Network -> ActiveLoanConfiguration -> IO (Either Text [LoanUTxO])
runQueryActiveLoans network cfg = do
    activeRes <- fetchActiveLoans

    case activeRes of
      Right activeLoans -> do
        return $ Right $ sortOn loanUTxOLoanAmount $ map fromAddressUTxO activeLoans
      Left err -> return $ Left err
  where
    -- Try to query the offers.
    fetchActiveLoans :: IO (Either Text [AddressUTxO])
    fetchActiveLoans = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryActiveLoans cfg) env)

-- | Get all information for a particular options wallet.
runQueryOptionsWallet :: OptionsWallet -> IO (Either Text OptionsWallet)
runQueryOptionsWallet optionsWallet@OptionsWallet{..} = do
    (txRes,optionsRes) <- 
      concurrently queryTxsConcurrently queryUTxOsWithRedundancies

    case (,) <$> txRes <*> optionsRes of
      Right (txs,optionsUTxOs) -> do
        return $ Right $ optionsWallet
          & #lovelace .~ sum (map (view #lovelace) optionsUTxOs)
          & #utxos .~ map fromAddressUTxO optionsUTxOs
          & populateNativeAssets
          & #transactions %~ mappend (map P2P.toTransaction txs)
      Left err -> return $ Left err
  where
    -- | Aggregate all native assets located at the wallet.
    populateNativeAssets :: OptionsWallet -> OptionsWallet
    populateNativeAssets s@OptionsWallet{utxos=us} =
      s & #nativeAssets .~ sumNativeAssets (concatMap (view #nativeAssets) us)

    -- Add one to the blockHeight for the most recently recorded transaction for this wallet.
    -- Koios will return any transactions for that block or later.
    afterBlock :: Integer
    afterBlock = (+1) $ maybe 0 (view #blockHeight) $ maybeHead transactions

    -- Try to query the options address' utxos.
    fetchUTxOs :: IO (Either Text [AddressUTxO])
    fetchUTxOs = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> handleTimeoutError (runClientM (queryAddressUTxOs [optionsAddress]) env)

    -- Since Koios instances may occassionally return incorrect UTxOs if they are not
    -- close enough to the chain tip, this would mess with the wallet's notifications.
    -- To account for this, the UTxOs are queried three times and compared. At least
    -- two responses must match to move on. The redundant queries occur concurrently.
    queryUTxOsWithRedundancies :: IO (Either Text [AddressUTxO])
    queryUTxOsWithRedundancies = queryWithRedundancies fetchUTxOs

    -- Try to query the tx hashes since last time. If a timeout error occurs, just try again.
    fetchTxHashes :: IO (Either Text [Text])
    fetchTxHashes = do
      manager <- newManager customTlsSettings
      let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
      first show <$> 
        handleTimeoutError (runClientM (queryAddressTxHashes [optionsAddress] afterBlock) env)

    -- Try to query the transaction info concurrently. The transactions are grouped together,
    -- 50 per response, since some transactions can be quite large.
    queryTxsConcurrently :: IO (Either Text [Transaction])
    queryTxsConcurrently = do
      fetchTxHashes >>= \case
        Left err -> return $ Left $ show err
        Right hashes -> do
          manager <- newManager customTlsSettings
          let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
          bimap show concat . sequence <$> 
            mapConcurrently 
              (\hs -> handleTimeoutError $ runClientM (queryAddressTransactions hs) env)
              (groupInto 70 hashes)

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
-- | Optionally filter by what is in the inline datum.
newtype InlineDatumFilterParam = InlineDatumFilterParam Text
  deriving newtype (ToHttpApiData,IsString)

-- | Optionally filter by what assets are present in the UTxO.
newtype AssetListFilterParam = AssetListFilterParam Text
  deriving newtype (ToHttpApiData,IsString)

-- | Offset the response.
newtype OffsetParam = OffsetParam Int
  deriving newtype (ToHttpApiData,Num)

-- | Order the response.
newtype OrderParam = OrderParam Text
  deriving newtype (ToHttpApiData,IsString)

-- | Which fields to return in the response.
newtype SelectParam = SelectParam Text
  deriving newtype (ToHttpApiData,IsString)

-- | Whether to only return UTxOs that still exist. This is either "eq.false" or "eq.true".
newtype IsSpentParam = IsSpentParam Text
  deriving newtype (ToHttpApiData,IsString)

type KoiosApi
  =     ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

  :<|>  "cli_protocol_params"
     :> Get '[JSON] Value

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "is_spent" IsSpentParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "order" OrderParam
     :> QueryParam "asset_list" AssetListFilterParam
     :> ReqBody '[JSON] PaymentAddressesExtended
     :> Post '[JSON] [AddressUTxO]

  :<|> "address_txs"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> ReqBody '[JSON] PaymentAddressesAfterBlock
     :> Post '[JSON] TxHashes

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "order" OrderParam
     :> ReqBody '[JSON] TxHashes
     :> Post '[JSON] [Transaction]

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "order" OrderParam
     :> ReqBody '[JSON] TxHashes
     :> Post '[JSON] [EventTransaction]

  :<|>  "account_info"
     :> QueryParam' '[Required] "select" SelectParam
     :> ReqBody '[JSON] StakeAddresses
     :> Post '[JSON] [StakeAccount]

  :<|>  "account_rewards"
     :> QueryParam' '[Required] "select" SelectParam
     :> ReqBody '[JSON] StakeAddresses
     :> Post '[JSON] [StakeRewards]

  :<|>  "pool_info"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam "sigma" Text
     :> QueryParam "meta_json" Text
     :> ReqBody '[JSON] Pools
     :> Post '[JSON] [Pool]

  :<|>  "account_addresses"
     :> QueryParam' '[Required] "select" SelectParam
     :> ReqBody '[JSON] StakeAddressesNonEmpty
     :> Post '[JSON] LinkedPaymentAddresses

  :<|> "pool_list"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "pool_status" Text
     :> Get '[JSON] Pools

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "is_spent" IsSpentParam
     :> QueryParam "asset_list" AssetListFilterParam
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [AddressUTxO]

  :<|>  "asset_history"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> Get '[JSON] [MintTransactions]

  :<|>  "asset_txs"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> QueryParam' '[Required] "_history" Bool
     :> QueryParam "_after_block_height" Integer
     :> Get '[JSON] [AssetTransaction]

  -- A version of asset_utxos that supports filtering AskUTxOs by what is in the datum.
  -- The inline datum is VERY specific to Ask UTxOs so this cannot be used with other UTxOs.
  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "is_spent" IsSpentParam
     -- Ada should always be the first asset in the collateral list if it is being used.
     :> QueryParam "inline_datum->value->fields->7->map->0->k->bytes" InlineDatumFilterParam
     -- The minimum loan duration.
     :> QueryParam "inline_datum->value->fields->6->int" InlineDatumFilterParam
     -- The maximum loan duration.
     :> QueryParam "inline_datum->value->fields->6->int" InlineDatumFilterParam
     :> QueryParam "asset_list" AssetListFilterParam
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [AddressUTxO]

  -- A version of asset_utxos that supports filtering OfferUTxOs by what is in the datum.
  -- The inline datum is VERY specific to Offer UTxOs so this cannot be used with other UTxOs.
  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "is_spent" IsSpentParam
     -- The minimum loan duration.
     :> QueryParam "inline_datum->value->fields->8->int" InlineDatumFilterParam
     -- The maximum loan duration.
     :> QueryParam "inline_datum->value->fields->8->int" InlineDatumFilterParam
     :> QueryParam "asset_list" AssetListFilterParam
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [AddressUTxO]

  -- A version of asset_utxos that supports filtering ActiveUTxOs by what is in the datum.
  -- The inline datum is VERY specific to Active UTxOs so this cannot be used with other UTxOs.
  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" SelectParam
     :> QueryParam' '[Required] "offset" OffsetParam
     :> QueryParam' '[Required] "is_spent" IsSpentParam
     -- The minimum loan duration.
     :> QueryParam "inline_datum->value->fields->11->int" InlineDatumFilterParam
     -- The maximum loan duration.
     :> QueryParam "inline_datum->value->fields->11->int" InlineDatumFilterParam
     :> QueryParam "asset_list" AssetListFilterParam
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [AddressUTxO]

submitApi
  :<|> evaluateApi 
  :<|> paramsApi
  :<|> addressUTxOsApi 
  :<|> addressTxsApi
  :<|> fullTxInfoApi 
  :<|> eventTxInfoApi 
  :<|> stakeAccountApi
  :<|> stakeRewardsApi
  :<|> poolInfoApi
  :<|> linkedPaymentAddressesApi
  :<|> poolListApi
  :<|> assetUTxOsApi
  :<|> mintHistoryApi
  :<|> assetTxHistoryApi
  :<|> askUTxOsApi
  :<|> offerUTxOsApi
  :<|> activeUTxOsApi
  = client (Proxy :: Proxy KoiosApi)

-- Query all UTxOs for a list of payment addresses.
queryAddressUTxOs :: [PaymentAddress] -> ClientM [AddressUTxO]
queryAddressUTxOs addrs = queryUTxOs 0 []
  where
    -- | This queries 1000 UTxOs at a time.
    queryUTxOs :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryUTxOs offset !acc = do
      !res <- 
        addressUTxOsApi 
          select 
          (IsSpentParam "eq.false")
          offset 
          (OrderParam "block_height.asc") 
          Nothing
          (PaymentAddressesExtended addrs)
      if length res == 1000 then 
        -- Query again since there may be more.
        queryUTxOs (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    select :: SelectParam
    select =
      fromString $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "stake_address"
        , "value"
        , "datum_hash"
        , "inline_datum"
        , "asset_list"
        , "reference_script"
        , "block_time"
        , "block_height"
        ]

-- Query all transactions for a list of addresses but only transactions at or after the specified
-- block.
queryAddressTxHashes :: [PaymentAddress] -> Integer -> ClientM [Text]
queryAddressTxHashes addrs lastBlock = queryHashes 0 []
  where
    queryHashes :: OffsetParam -> [Text] -> ClientM [Text]
    queryHashes offset !acc = do
      (TxHashes hashes) <- 
        -- Only fetch the "tx_hash" column.
        addressTxsApi "tx_hash" offset (PaymentAddressesAfterBlock addrs lastBlock)
      if length hashes == 1000 then 
        -- Query again since there may be more.
        queryHashes (offset + 1000) $ acc <> hashes
      else
        -- That should be the last of the results.
        return $ acc <> hashes

-- Query the information for a list of transaction hashes. The transactions are returned latest
-- first.
queryAddressTransactions :: [Text] -> ClientM [Transaction]
queryAddressTransactions = fullTxInfoApi select "block_height.desc" . TxHashes
  where
    select :: SelectParam
    select = 
      fromString $ intercalate ","
        [ "tx_hash"
        , "tx_timestamp"
        , "block_height"
        , "fee"
        , "tx_size"
        , "deposit"
        , "invalid_before"
        , "invalid_after"
        , "collateral_inputs"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        , "certificates"
        , "withdrawals"
        , "assets_minted"
        -- , "native_scripts"
        , "plutus_contracts" 
        ]

queryStakeAccounts :: [StakeAddress] -> ClientM [StakeAccount]
queryStakeAccounts addrs = stakeAccountApi select $ StakeAddresses addrs
  where
    select :: SelectParam
    select =
      fromString $ intercalate ","
        [ "stake_address"
        , "status"
        , "delegated_pool"
        , "total_balance"
        , "utxo"
        , "rewards_available"
        ]

queryStakeRewards :: [StakeAddress] -> ClientM [StakeReward]
queryStakeRewards addrs = 
    concatMap unStakeRewards <$> stakeRewardsApi "rewards" (StakeAddresses addrs)

-- Some pools will have `Nothing` for a lot of correlated fields. The sigma filter
-- can be used to filter out these pools. Another possible filter is whethere a pool
-- has registered metadata.
queryPoolInfo :: Maybe Text -> Maybe Text -> Pools -> ClientM [Pool]
queryPoolInfo _ _ (Pools []) = return []
queryPoolInfo sigmaFilter metaFilter pools = poolInfoApi select sigmaFilter metaFilter pools
  where
    select :: SelectParam
    select =
      fromString $ intercalate ","
        [ "pool_id_bech32"
        , "margin"
        , "fixed_cost"
        , "pledge"
        , "meta_json"
        , "pool_status"
        , "retiring_epoch"
        , "active_stake"
        , "sigma"
        , "block_count"
        , "live_pledge"
        , "live_stake"
        , "live_delegators"
        , "live_saturation"
        ]

queryLinkedPaymentAddresses :: [StakeAddress] -> ClientM [PaymentAddress]
queryLinkedPaymentAddresses addrs = 
  unLinkedPaymentAddresses <$> linkedPaymentAddressesApi "addresses" (StakeAddressesNonEmpty addrs)

queryPoolIds :: OffsetParam -> [PoolID] -> ClientM [PoolID]
queryPoolIds offset !acc = do
  (Pools poolIds) <- poolListApi "pool_id_bech32,pool_status" offset "eq.registered"
  if length poolIds == 1000 then
    -- Query again since there may be more.
    queryPoolIds (offset + 1000) $ acc <> poolIds
  else
    -- That should be the last of the results.
    return $ acc <> poolIds

queryOneWaySwaps :: OfferAsset -> AskAsset -> ClientM [AddressUTxO]
queryOneWaySwaps offerAsset askAsset = queryApi 0 []
  where
    offerFilter :: Maybe AssetListFilterParam
    offerFilter 
      | offerAsset ^. #unOfferAsset % #policyId == "" = Nothing
      | otherwise = Just $ AssetListFilterParam $ 
          "cs." <> assetToQueryParam (unOfferAsset offerAsset)

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- assetUTxOsApi select offset "eq.false" offerFilter $ 
        AssetList [(OneWay.beaconCurrencySymbol, OneWay.genPairBeaconName offerAsset askAsset)]
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    assetToQueryParam :: NativeAsset -> Text
    assetToQueryParam NativeAsset{policyId,tokenName} = mconcat
      [ "[{\"policy_id\":\""
      , display policyId
      , "\",\"asset_name\":\""
      , display tokenName
      , "\"}]"
      ]

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]

queryTwoWaySwaps :: OfferAsset -> AskAsset -> ClientM [AddressUTxO]
queryTwoWaySwaps (OfferAsset offerAsset) (AskAsset askAsset) = queryApi 0 []
  where
    offerFilter :: Maybe AssetListFilterParam
    offerFilter 
      | offerAsset ^. #policyId == "" = Nothing
      | otherwise = Just $ AssetListFilterParam $ "cs." <> assetToQueryParam offerAsset

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- assetUTxOsApi select offset "eq.false" offerFilter $ 
        AssetList [(TwoWay.beaconCurrencySymbol, TwoWay.genPairBeaconName offerAsset askAsset)]
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    assetToQueryParam :: NativeAsset -> Text
    assetToQueryParam NativeAsset{policyId,tokenName} = mconcat
      [ "[{\"policy_id\":\""
      , display policyId
      , "\",\"asset_name\":\""
      , display tokenName
      , "\"}]"
      ]

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]

queryLoanAsks :: LoanAskConfiguration -> ClientM [AddressUTxO]
queryLoanAsks LoanAskConfiguration{..} = queryApi 0 []
  where
    minDurationFilter :: Maybe InlineDatumFilterParam
    minDurationFilter = 
      minDuration <&> 
        InlineDatumFilterParam . ("gte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    maxDurationFilter :: Maybe InlineDatumFilterParam
    maxDurationFilter = 
      maxDuration <&> 
        InlineDatumFilterParam . ("lte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    collateralAsAssets :: [Loans.Asset]
    collateralAsAssets = map fromNativeAsset collateral

    assetBeaconAsAsset :: [Loans.Asset]
    assetBeaconAsAsset = case loanAsset of
      Nothing -> []
      Just asset -> 
        let (Loans.AssetBeaconId assetBeacon) = 
              Loans.genLoanAssetBeaconName $ fromNativeAsset asset
         in [Loans.Asset (Loans.negotiationBeaconCurrencySymbol, assetBeacon)]

    adaAsset :: Loans.Asset
    adaAsset = fromNativeAsset lovelaceAsNativeAsset

    adaCollateralFilter :: Maybe InlineDatumFilterParam
    adaCollateralFilter
      | adaAsset `elem` collateralAsAssets = Just $ InlineDatumFilterParam "eq.\"\""
      | otherwise = Nothing
    
    -- Assets that must be present, besides the Ask beacon.
    -- If ada is used as collateral, the filtering is uniquely handled.
    requiredExtraAssets :: [Loans.Asset]
    requiredExtraAssets = assetBeaconAsAsset <> filter (/= adaAsset) collateralAsAssets

    collateralFilter
      | null requiredExtraAssets  = Nothing
      | otherwise = Just 
                  $ AssetListFilterParam 
                  $ assetToQueryParam requiredExtraAssets

    assetToQueryParam :: [Loans.Asset] -> Text
    assetToQueryParam assets = "cs.[" <> mconcat (intersperse "," (go assets)) <> "]"
      where
        go :: [Loans.Asset] -> [Text]
        go [] = []
        go (Loans.Asset (currSym,tokName):xs) = 
          let policyId = display currSym
              assetName = display tokName
           in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]

    targetBeacon :: AssetList
    targetBeacon = AssetList [(Loans.negotiationBeaconCurrencySymbol, Loans.askBeaconName)]

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- askUTxOsApi 
        select 
        offset 
        "eq.false" 
        adaCollateralFilter 
        minDurationFilter 
        maxDurationFilter
        collateralFilter 
        targetBeacon
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

queryLoanOffers :: LoanOfferConfiguration -> ClientM [AddressUTxO]
queryLoanOffers LoanOfferConfiguration{..} = queryApi 0 []
  where
    lenderIdAsAsset :: [Loans.Asset]
    lenderIdAsAsset = case lenderCredential of
      Nothing -> []
      Just cred -> 
        let (Loans.LenderId lenderId) = Loans.genLenderId cred
         in [Loans.Asset (Loans.negotiationBeaconCurrencySymbol, lenderId)]

    minDurationFilter :: Maybe InlineDatumFilterParam
    minDurationFilter = 
      minDuration <&> 
        InlineDatumFilterParam . ("gte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    maxDurationFilter :: Maybe InlineDatumFilterParam
    maxDurationFilter = 
      maxDuration <&> 
        InlineDatumFilterParam . ("lte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    assetBeaconAsAsset :: [Loans.Asset]
    assetBeaconAsAsset = case loanAsset of
      Nothing -> []
      Just asset -> 
        let (Loans.AssetBeaconId assetBeacon) = 
              Loans.genLoanAssetBeaconName $ fromNativeAsset asset
         in [Loans.Asset (Loans.negotiationBeaconCurrencySymbol, assetBeacon)]

    -- Assets that must be present, besides the Offer beacon.
    requiredExtraAssets :: [Loans.Asset]
    requiredExtraAssets = lenderIdAsAsset <> assetBeaconAsAsset

    extraAssetFilter :: Maybe AssetListFilterParam
    extraAssetFilter
      | null requiredExtraAssets = Nothing
      | otherwise = Just 
                  $ AssetListFilterParam 
                  $ assetToQueryParam requiredExtraAssets

    targetBeacon :: AssetList
    targetBeacon = AssetList [(Loans.negotiationBeaconCurrencySymbol, Loans.offerBeaconName)]

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- offerUTxOsApi 
        select 
        offset 
        "eq.false" 
        minDurationFilter 
        maxDurationFilter
        extraAssetFilter 
        targetBeacon
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    assetToQueryParam :: [Loans.Asset] -> Text
    assetToQueryParam assets = "cs.[" <> mconcat (intersperse "," (go assets)) <> "]"
      where
        go :: [Loans.Asset] -> [Text]
        go [] = []
        go (Loans.Asset (currSym,tokName):xs) = 
          let policyId = display currSym
              assetName = display tokName
           in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]

-- Query all transactions involving a given asset.
queryAssetTxHashes :: CurrencySymbol -> TokenName -> Maybe Integer -> ClientM [Text]
queryAssetTxHashes policyId name mAfterBlock = queryHashes 0 []
  where
    queryHashes :: OffsetParam -> [Text] -> ClientM [Text]
    queryHashes offset !acc = do
      hashes <- map (view #txHash) <$>
        -- Only fetch the "tx_hash" column.
        assetTxHistoryApi "tx_hash" policyId name True mAfterBlock
      if length hashes == 1000 then 
        -- Query again since there may be more.
        queryHashes (offset + 1000) $ acc <> hashes
      else
        -- That should be the last of the results.
        return $ acc <> hashes

-- | Query all transactions for a specific loan id.
queryLoanTxHistory :: Loans.LoanId -> ClientM [LoanEvent]
queryLoanTxHistory i@(Loans.LoanId loanId) = do
    info <- queryAssetTxHashes Loans.activeBeaconCurrencySymbol loanId Nothing >>= 
      eventTxInfoApi select "block_height.asc" . TxHashes
    return $ mapMaybe (toLoanEvent i) info
  where
    select = SelectParam $ toText $ intercalate ","
      [ "tx_hash"
      , "tx_timestamp"
      , "block_height"
      , "inputs"
      , "outputs"
      , "assets_minted"
      , "plutus_contracts"
      ]

queryBorrowerCreditHistory :: Loans.BorrowerId -> ClientM [LoanResult]
queryBorrowerCreditHistory b@(Loans.BorrowerId borrowerId) = do
    txs <- concatMap unMintTransactions <$>
      mintHistoryApi "minting_txs" Loans.activeBeaconCurrencySymbol borrowerId
    let processedTxs = map (view #txHash) -- Get the tx hash.
                     $ filter ((<0) . view #quantity) txs -- Get only burn transactions.
    info <- eventTxInfoApi select "block_height.desc" $ TxHashes processedTxs
    return $ mapMaybe (toLoanResult b) info
  where
    select = SelectParam $ toText $ intercalate ","
      [ "tx_hash"
      , "tx_timestamp"
      , "block_height"
      , "inputs"
      , "outputs"
      , "assets_minted"
      , "plutus_contracts"
      ]

querySpecificLoanUTxO :: Loans.LoanId -> ClientM [AddressUTxO]
querySpecificLoanUTxO (Loans.LoanId loanId) = queryApi 0 []
  where
    activeBeaconTarget = mkNativeAsset Loans.activeBeaconCurrencySymbol Loans.activeBeaconName

    assetFilter :: Maybe AssetListFilterParam
    assetFilter = Just 
                $ AssetListFilterParam 
                $ "cs." <> assetToQueryParam activeBeaconTarget

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- assetUTxOsApi select offset "eq.false" assetFilter $ 
        AssetList [(Loans.activeBeaconCurrencySymbol, loanId)]
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    assetToQueryParam :: NativeAsset -> Text
    assetToQueryParam NativeAsset{policyId,tokenName} = mconcat
      [ "[{\"policy_id\":\""
      , display policyId
      , "\",\"asset_name\":\""
      , display tokenName
      , "\"}]"
      ]

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]

queryActiveLoans :: ActiveLoanConfiguration -> ClientM [AddressUTxO]
queryActiveLoans ActiveLoanConfiguration{..} = queryApi 0 []
  where
    minDurationFilter :: Maybe InlineDatumFilterParam
    minDurationFilter = 
      minDuration <&> 
        InlineDatumFilterParam . ("gte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    maxDurationFilter :: Maybe InlineDatumFilterParam
    maxDurationFilter = 
      maxDuration <&> 
        InlineDatumFilterParam . ("lte." <>) . display . toPlutusTime . convertDaysToPosixPeriod

    assetBeaconAsAsset :: [Loans.Asset]
    assetBeaconAsAsset = case loanAsset of
      Nothing -> []
      Just asset -> 
        let (Loans.AssetBeaconId assetBeacon) = 
              Loans.genLoanAssetBeaconName $ fromNativeAsset asset
         in [Loans.Asset (Loans.activeBeaconCurrencySymbol, assetBeacon)]

    -- Assets that must be present, besides the Offer beacon.
    requiredExtraAssets :: [Loans.Asset]
    requiredExtraAssets = assetBeaconAsAsset

    extraAssetFilter :: Maybe AssetListFilterParam
    extraAssetFilter
      | null requiredExtraAssets = Nothing
      | otherwise = Just 
                  $ AssetListFilterParam 
                  $ assetToQueryParam requiredExtraAssets

    targetBeacon :: AssetList
    targetBeacon = AssetList [(Loans.activeBeaconCurrencySymbol, Loans.activeBeaconName)]

    queryApi :: OffsetParam -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryApi offset !acc = do
      res <- activeUTxOsApi 
        select 
        offset 
        "eq.false" 
        minDurationFilter 
        maxDurationFilter
        extraAssetFilter 
        targetBeacon
      if length res == 1000 then
        -- Query again since there may be more.
        queryApi (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    assetToQueryParam :: [Loans.Asset] -> Text
    assetToQueryParam assets = "cs.[" <> mconcat (intersperse "," (go assets)) <> "]"
      where
        go :: [Loans.Asset] -> [Text]
        go [] = []
        go (Loans.Asset (currSym,tokName):xs) = 
          let policyId = display currSym
              assetName = display tokName
           in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs

    select :: SelectParam
    select = fromString $ intercalate ","
      [ "is_spent"
      , "tx_hash"
      , "tx_index"
      , "address"
      , "stake_address"
      , "value"
      , "datum_hash"
      , "inline_datum"
      , "asset_list"
      , "reference_script"
      , "block_time"
      , "block_height"
      ]
