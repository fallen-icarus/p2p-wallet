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
  ) where

import Servant.Client (client , ClientM , runClientM , Scheme(Https) , BaseUrl(..) , mkClientEnv)
import Servant.Client qualified as Client
import Servant.API ((:<|>)(..), JSON, Post, Get, (:>), ReqBody, Required, QueryParam', QueryParam)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.HTTP.Client (newManager)
import Data.Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import UnliftIO.Async (mapConcurrently,concurrently)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.LinkedPaymentAddresses
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
    processResults val = (,) <$> pure (valueAsByteString val) <*> parseCollateralPercentage val

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
          wallet & #utxos .~ map toPersonalUTxO us
                 & #lovelace .~ sum (map (view #lovelace) us)
                 & #transactions %~ mappend (map (P2P.toTransaction profileId paymentId) txs)
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
    queryUTxOsWithRedundancies = do
      (res1,(res2,res3)) <- concurrently fetchUTxOs (concurrently fetchUTxOs fetchUTxOs)
      let path1 = liftA3 (\a b c -> a == b || a == c) res1 res2 res3
          path2 = liftA2 (==) res2 res3
      case (path1,path2) of
        (Right True ,_) -> return $ first show res1
        (_, Right True) -> return $ first show res2
        (Left err, _) -> return $ Left $ show err
        (_, Left err) -> return $ Left $ show err
        _ -> return $ Left "There was an error syncing UTxOs. Wait a few seconds and try again."

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
              (groupInto 50 hashes)

-- | Sync the latest information for the stake wallet. Try to do as much concurrently as possible.
runQueryStakeWalletInfo :: StakeWallet -> IO (Either Text StakeWallet)
runQueryStakeWalletInfo wallet@StakeWallet{network,profileId,stakeId,stakeAddress} = do
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
                     & #rewardHistory .~ reverse (map (P2P.toStakeReward profileId stakeId) rewards)
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
          map toSwapUTxO $ oneWayUTxOs <> twoWayUTxOs
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
runQueryDexWallet dexWallet@DexWallet{network,oneWaySwapAddress,twoWaySwapAddress} = do
    (oneWayRes,twoWayRes) <- concurrently fetchOneWaySwaps fetchTwoWaySwaps

    case (,) <$> oneWayRes <*> twoWayRes of
      Right (oneWayUTxOs,twoWayUTxOs) -> do
        let allUTxOs = oneWayUTxOs <> twoWayUTxOs
        return $ Right $ dexWallet
          & #lovelace .~ sum (map (view #lovelace) allUTxOs)
          & #utxos .~ map toSwapUTxO allUTxOs
          & populateNativeAssets
      Left err -> return $ Left err
  where
    -- | Aggregate all native assets located at the wallet.
    populateNativeAssets :: DexWallet -> DexWallet
    populateNativeAssets s@DexWallet{utxos=us} =
      s & #nativeAssets .~ sumNativeAssets (concatMap (view #nativeAssets) us)

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

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
type KoiosApi
  =     ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

  :<|>  "cli_protocol_params"
     :> Get '[JSON] Value

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam' '[Required] "order" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] PaymentAddressesExtended
     :> Post '[JSON] [AddressUTxO]

  :<|> "address_txs"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> ReqBody '[JSON] PaymentAddressesAfterBlock
     :> Post '[JSON] TxHashes

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "order" Text
     :> ReqBody '[JSON] TxHashes
     :> Post '[JSON] [Transaction]

  :<|>  "account_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] StakeAddresses
     :> Post '[JSON] [StakeAccount]

  :<|>  "account_rewards"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] StakeAddresses
     :> Post '[JSON] [StakeRewards]

  :<|>  "pool_info"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam "sigma" Text
     :> QueryParam "meta_json" Text
     :> ReqBody '[JSON] Pools
     :> Post '[JSON] [Pool]

  :<|>  "account_addresses"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] StakeAddressesNonEmpty
     :> Post '[JSON] LinkedPaymentAddresses

  :<|> "pool_list"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam' '[Required] "pool_status" Text
     :> Get '[JSON] Pools

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [AddressUTxO]

submitApi
  :<|> evaluateApi 
  :<|> paramsApi
  :<|> addressUTxOsApi 
  :<|> addressTxsApi
  :<|> txInfoApi 
  :<|> stakeAccountApi
  :<|> stakeRewardsApi
  :<|> poolInfoApi
  :<|> linkedPaymentAddressesApi
  :<|> poolListApi
  :<|> assetUTxOsApi
  = client (Proxy :: Proxy KoiosApi)

-- Query all UTxOs for a list of payment addresses.
queryAddressUTxOs :: [PaymentAddress] -> ClientM [AddressUTxO]
queryAddressUTxOs addrs = queryUTxOs 0 []
  where
    -- | This queries 1000 UTxOs at a time.
    queryUTxOs :: Int -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryUTxOs offset !acc = do
      !res <- 
        addressUTxOsApi 
          select 
          "eq.false" -- not spent
          offset 
          "block_height.asc"  -- ordering by block_height
          Nothing -- no limit
          (PaymentAddressesExtended addrs)
      if length res == 1000 then 
        -- Query again since there may be more.
        queryUTxOs (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    select :: Text
    select =
      mconcat $ intersperse ","
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
    queryHashes :: Int -> [Text] -> ClientM [Text]
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
queryAddressTransactions = txInfoApi select "block_height.desc" . TxHashes
  where
    select :: Text
    select = 
      mconcat $ intersperse ","
        [ "tx_hash"
        , "tx_timestamp"
        , "block_height"
        , "fee"
        , "tx_size"
        , "deposit"
        , "invalid_before"
        , "invalid_after"
        , "collateral_inputs"
        -- , "collateral_output"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        , "certificates"
        , "withdrawals"
        , "assets_minted"
        -- , "native_scripts"
        -- , "plutus_contracts" 
        ]

queryStakeAccounts :: [StakeAddress] -> ClientM [StakeAccount]
queryStakeAccounts addrs = stakeAccountApi select $ StakeAddresses addrs
  where
    select =
      toText $ intercalate ","
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
    select =
      toText $ intercalate ","
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

queryPoolIds :: Int -> [PoolID] -> ClientM [PoolID]
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
    offerFilter :: Maybe Text
    offerFilter 
      | offerAsset ^. #unOfferAsset % #policyId == "" = Nothing
      | otherwise = Just $ "cs." <> assetToQueryParam (unOfferAsset offerAsset)

    queryApi :: Int -> [AddressUTxO] -> ClientM [AddressUTxO]
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

    select :: Text
    select = fromString $ mconcat $ intersperse ","
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
    offerFilter :: Maybe Text
    offerFilter 
      | offerAsset ^. #policyId == "" = Nothing
      | otherwise = Just $ "cs." <> assetToQueryParam offerAsset

    queryApi :: Int -> [AddressUTxO] -> ClientM [AddressUTxO]
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

    select :: Text
    select = fromString $ mconcat $ intersperse ","
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
