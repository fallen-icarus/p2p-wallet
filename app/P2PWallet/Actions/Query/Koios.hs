{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module P2PWallet.Actions.Query.Koios
  (
    runSubmitTx
  , runEvaluateTx
  , runQueryPaymentWalletInfo
  , runQueryStakeWalletInfo
  , runQueryAllRegisteredPools
  ) where

import Servant.Client 
  ( client
  , ClientM
  , runClientM
  , Scheme(Https)
  , BaseUrl(..)
  , mkClientEnv
  )
import Servant.Client qualified as Client
import Servant.API ((:<|>)(..), JSON, Post, Get, (:>), ReqBody, Required, QueryParam', QueryParam)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson 
import Data.Text qualified as Text
import UnliftIO.Async (mapConcurrently)

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.LinkedPaymentAddresses
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Koios.PostTypes
import P2PWallet.Data.Koios.StakeAccount
import P2PWallet.Data.Koios.StakeReward
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Transaction qualified as P2P
import P2PWallet.Data.StakeReward qualified as P2P
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- High-Level API
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

runSubmitTx :: Network -> FilePath -> IO (Either Text Value)
runSubmitTx network txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file."
    Just tx -> do
      manager <- newManager tlsManagerSettings
      res <-
        runClientM (submitApi $ SubmitTxCBOR tx) $
          mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

runEvaluateTx :: Network -> FilePath -> IO (Either Text Value)
runEvaluateTx network txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file"
    Just tx -> do
      manager <- newManager tlsManagerSettings
      res <-
        runClientM (evaluateApi $ EvaluateTxCBOR tx) $
          mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

runQueryPaymentWalletInfo :: Network -> PaymentWallet -> IO (Either Text PaymentWallet)
runQueryPaymentWalletInfo network wallet@PaymentWallet{paymentId,profileId} = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
        lastBlock = fromMaybe 0 $ fmap (view #blockHeight) $ maybeHead $ wallet ^. #transactions
    runClientM (queryPaymentWalletInfo (wallet ^. #paymentAddress) lastBlock) env >>= \case
      Right (us,txs,as) -> do
        return $ Right $ 
          wallet & #utxos .~ map toPersonalUTxO us
                 & #lovelace .~ sum (map (view #lovelace) us)
                 & #transactions %~ mappend 
                     (sortOn (negate . view #blockTime) $ map (P2P.toTransaction profileId paymentId) txs)
                 & #nativeAssets .~ as
      Left err -> do
        let errText = show @Text err
        if "ResponseTimeout)" == Text.takeEnd 16 errText then
          return $ Left "Server timed out"
        else
          return $ Left errText

runQueryStakeWalletInfo :: Network -> StakeWallet -> IO (Either Text StakeWallet)
runQueryStakeWalletInfo network' wallet@StakeWallet{stakeId,profileId} = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1")
  res <- runClientM (queryStakeWalletInfo $ wallet ^. #stakeAddress) env
  case res of
    Right ([StakeAccount{..}],rewards,pools,linkedAddresses) -> 
      return $ Right $
        wallet & #registrationStatus .~ registrationStatus
               & #totalDelegation .~ totalDelegation
               & #utxoBalance .~ utxoBalance
               & #availableRewards .~ availableRewards
               & #delegatedPool .~ maybeHead pools
               & #rewardHistory .~ map (P2P.toStakeReward profileId stakeId) rewards
               & #linkedAddresses .~ linkedAddresses
    Right ([],_,_,_) -> 
      -- If a stake address has never been seen on chain before (ie, a UTxO has not been created 
      -- at a payment address using the staking credential), the query will return the empty list.
      -- The preset fields for the account are accurate in this scenario.
      return $ Right wallet
    Right _ -> do
      return $ Left "Stake wallet query returned an unexpected number of arguments."
    Left err -> return $ Left $ show err

runQueryAllRegisteredPools :: Network -> IO (Either Text [Pool])
runQueryAllRegisteredPools network = do
    manager' <- newManager tlsManagerSettings
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

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
type KoiosApi
  =     ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [AddressUTxO]

  :<|> "address_txs"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> ReqBody '[JSON] PaymentAddressesAfterBlock
     :> Post '[JSON] TxHashes

  :<|> "address_assets"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> ReqBody '[JSON] PaymentAddresses
     :> Post '[JSON] [NativeAsset]

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" Text
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
     :> ReqBody '[JSON] NonEmptyStakeAddresses
     :> Post '[JSON] LinkedPaymentAddresses

  :<|> "pool_list"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam' '[Required] "pool_status" Text
     :> Get '[JSON] Pools

submitApi
  :<|> evaluateApi 
  :<|> addressUTxOsApi 
  :<|> addressTxsApi
  :<|> addressAssetsApi 
  :<|> txInfoApi 
  :<|> stakeAccountApi
  :<|> stakeRewardsApi
  :<|> poolInfoApi
  :<|> linkedPaymentAddressesApi
  :<|> poolListApi
  = client (Proxy :: Proxy KoiosApi)

queryPaymentWalletInfo 
  :: PaymentAddress 
  -> Integer 
  -> ClientM ([AddressUTxO],[Transaction],[NativeAsset])
queryPaymentWalletInfo addr lastBlock = 
  (,,) <$> queryAddressUTxOs [addr]
       <*> queryAddressTransactions [addr] lastBlock
       <*> queryAddressAssets [addr]

queryStakeWalletInfo :: StakeAddress -> ClientM ([StakeAccount],[StakeReward],[Pool],[PaymentAddress])
queryStakeWalletInfo addr = do
  acc <- queryStakeAccounts [addr]

  (acc,,,) 
    <$> queryStakeRewards [addr] 
    <*> (queryPoolInfo Nothing Nothing $ Pools $ catMaybes $ map (view #delegatedPool) acc)
    <*> queryLinkedPaymentAddresses [addr] 

queryAddressUTxOs :: [PaymentAddress] -> ClientM [AddressUTxO]
queryAddressUTxOs addrs = do
    -- Since koios may occassionally return incorrect UTxOs if the instance queried is not properly
    -- synced, this would create a lot of problems for this program. To account for this, the
    -- address UTxOs are queried up to three times; the results should match for at least two of
    -- them. If they don't match, query another three and try again.
    (r1,r2) <- 
      (,) <$> queryUTxOs 0 []
          <*> queryUTxOs 0 []

    -- Only query a third time if necessary.
    if r1 == r2 then 
      -- Two results matched so return one of them.
      return r1 
    else do
      -- Query a third time and compare against the previous two.
      r3 <- queryUTxOs 0 []
      if r3 == r1 || r3 == r2 then 
        -- At least two matched.
        return r3
      else 
        -- Query another three times since they did not match.
        queryAddressUTxOs addrs

  where
    queryUTxOs :: Int -> [AddressUTxO] -> ClientM [AddressUTxO]
    queryUTxOs offset !acc = do
      !res <- addressUTxOsApi select "eq.false" offset Nothing (ExtendedPaymentAddresses addrs)
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

queryAddressAssets :: [PaymentAddress] -> ClientM [NativeAsset]
queryAddressAssets addrs = do
    -- Since koios may occassionally return incorrect results if the instance queried is not
    -- properly synced, this would create a lot of problems for this program. To account for this,
    -- the address UTxOs are queried three times; the results should match for at least two of them.
    -- If they don't match, query another three and try again.
    (r1,r2) <- 
      (,) <$> queryAssets 0 []
          <*> queryAssets 0 []

    -- Only query a third time if necessary.
    if r1 == r2 then 
      -- Two results matched so return one of them.
      return r1 
    else do
      -- Query a third time and compare against the previous two.
      r3 <- queryAssets 0 []
      if r3 == r1 || r3 == r2 then 
        -- At least two matched.
        return r3
      else 
        -- Query another three times since they did not match.
        queryAddressAssets addrs

  where
    queryAssets :: Int -> [NativeAsset] -> ClientM [NativeAsset]
    queryAssets offset !acc = do
      !res <- addressAssetsApi select offset (PaymentAddresses addrs)
      if length res == 1000 then 
        -- Query again since there may be more.
        queryAssets (offset + 1000) $ acc <> res
      else
        -- That should be the last of the results.
        return $ acc <> res

    select :: Text
    select =
      mconcat $ intersperse ","
        [ "policy_id"
        , "asset_name"
        , "fingerprint"
        , "quantity"
        ]

queryAddressTransactions :: [PaymentAddress] -> Integer -> ClientM [Transaction]
queryAddressTransactions addrs lastBlock =
    queryTxHashes 0 [] >>= 
      -- Tx info can be quite large so only 50 transactions are queried at a time.
      fmap concat . mapM (txInfoApi select . TxHashes) . groupInto 50
  where
    queryTxHashes :: Int -> [Text] -> ClientM [Text]
    queryTxHashes offset !acc = do
      (TxHashes hashes) <- 
        addressTxsApi "tx_hash" offset (PaymentAddressesAfterBlock addrs lastBlock)
      if length hashes == 1000 then 
        -- Query again since there may be more.
        queryTxHashes (offset + 1000) $ acc <> hashes
      else
        -- That should be the last of the results.
        return $ acc <> hashes

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
        -- , "assets_minted"
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
    concatMap unStakeRewards <$> 
      stakeRewardsApi select (StakeAddresses addrs)
  where
    select =
      toText $ intercalate ","
        [ "rewards" ]

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
    unLinkedPaymentAddresses <$> linkedPaymentAddressesApi select (NonEmptyStakeAddresses addrs)
  where
    select =
      toText $ intercalate ","
        [ "addresses"
        ]

queryPoolIds :: Int -> [PoolID] -> ClientM [PoolID]
queryPoolIds offset !acc = do
  (Pools poolIds) <- poolListApi "pool_id_bech32,pool_status" offset "eq.registered"
  if length poolIds == 1000 then
    -- Query again since there may be more.
    queryPoolIds (offset + 1000) $ acc <> poolIds
  else
    -- That should be the last of the results.
    return $ acc <> poolIds

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- Break a list into sublists of the specified length.
groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = 
  take n xs : groupInto n (drop n xs)
