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
  , runQueryUnknownUTxOInfo
  , runQueryAllRegisteredPools
  ) where

import Servant.Client (client, ClientM, runClientM, Scheme(Https), BaseUrl(..), mkClientEnv)
import Servant.Client qualified as Client
import Servant.API ((:<|>)(..), JSON, Post, Get, (:>), ReqBody, Required, QueryParam', QueryParam)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson 

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.PostTypes
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Koios.StakeAccount
import P2PWallet.Data.Koios.StakeReward
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- High-Level API
-------------------------------------------------
toNetworkURL :: Network -> String
toNetworkURL Mainnet = "api.koios.rest"
toNetworkURL Testnet = "preprod.koios.rest"

runSubmitTx :: Network -> FilePath -> IO (Either Text Value)
runSubmitTx network' txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file."
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <-
        runClientM (submitApi $ SubmitTxCBOR tx) $
          mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

runEvaluateTx :: Network -> FilePath -> IO (Either Text Value)
runEvaluateTx network' txFile = do
  tx' <- decode @TxCBOR <$> readFileLBS txFile
  case tx' of
    Nothing -> return $ Left "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <-
        runClientM (evaluateApi $ EvaluateTxCBOR tx) $
          mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1/ogmios")
      case res of
        Right r -> return $ Right r
        Left e@(Client.FailureResponse _ err) -> case decode $ Client.responseBody err of
          Just response -> return $ Right response
          Nothing -> return $ Left $ show e
        Left err -> return $ Left $ show err

runQueryPaymentWalletInfo :: Network -> PaymentWallet -> IO (Either Text PaymentWallet)
runQueryPaymentWalletInfo network' wallet = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1")
  res <- runClientM (queryPaymentWalletInfo $ wallet ^. paymentAddress) env
  case res of
    Right (us,txs,as) -> 
      return $ Right $ 
        wallet & utxos .~ us
               & lovelaces .~ sum (map (view lovelaces) us)
               & txHistory .~ txs
               & nativeAssets .~ as
    Left err -> return $ Left $ show err

runQueryStakeWalletInfo :: Network -> StakeWallet -> IO (Either Text StakeWallet)
runQueryStakeWalletInfo network' account = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1")
  res <- runClientM (queryStakeWalletInfo $ account ^. stakeAddress) env
  case res of
    Right ([StakeAccount{..}],rewards,pools) -> 
      return $ Right $ 
        account & registrationStatus .~ _registrationStatus
                & totalDelegation .~ _totalDelegation
                & utxoBalance .~ _utxoBalance
                & availableRewards .~ _availableRewards
                & delegatedPool .~ maybeHead pools
                & rewardHistory .~ concat rewards
    Right _ -> return $ Left "Query returned an unexpected number of arguments."
    Left err -> return $ Left $ show err

runQueryUnknownUTxOInfo :: Network -> [TxOutRef] -> IO (Either Text [AddressUTxO])
runQueryUnknownUTxOInfo network' refs = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1")
  res <- runClientM (queryUnknownUTxOs refs) env
  case res of
    Right rs -> return $ Right rs
    Left err -> return $ Left $ show err
  
runQueryAllRegisteredPools :: Network -> IO (Either Text [Pool])
runQueryAllRegisteredPools network' = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (toNetworkURL network') 443 "api/v1")
  res <- runClientM queryAllRegisteredPools env
  case res of
    Right rs -> return $ Right rs
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
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [AddressUTxO]

  :<|> "address_txs"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] PaymentAddresses
     :> Post '[JSON] TxHashes

  :<|> "address_assets"
     :> QueryParam' '[Required] "select" Text
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

  :<|> "utxo_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] UnknownUTxOs
     :> Post '[JSON] [AddressUTxO]

  :<|>  "account_rewards"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] StakeAddresses
     :> Post '[JSON] [[StakeReward]]

  :<|> "pool_list"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "offset" Int
     :> QueryParam' '[Required] "pool_status" Text
     :> Get '[JSON] Pools

  :<|>  "pool_info"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam "sigma" Text
     :> QueryParam "meta_json" Text
     :> ReqBody '[JSON] Pools
     :> Post '[JSON] [Pool]

submitApi :<|>
  evaluateApi :<|> 
  addressUTxOsApi :<|> 
  addressTxsApi :<|> 
  addressAssetsApi :<|> 
  txInfoApi :<|>
  stakeAccountApi :<|>
  utxoInfoApi :<|>
  stakeRewardsApi :<|> 
  poolListApi :<|> 
  poolInfoApi = client (Proxy :: Proxy KoiosApi)

queryPaymentWalletInfo :: PaymentAddress -> ClientM ([AddressUTxO],[Transaction],[NativeAsset])
queryPaymentWalletInfo addr = 
  (,,) <$> queryAddressUTxOs [addr]
       <*> queryAddressTransactions [addr]
       <*> queryAddressAssets [addr]

queryStakeWalletInfo :: StakeAddress -> ClientM ([StakeAccount],[[StakeReward]],[Pool])
queryStakeWalletInfo addr = do
  acc <- queryStakeAccounts [addr]

  (acc,,) 
    <$> queryStakeRewards [addr] 
    <*> (queryPoolInfo Nothing Nothing $ Pools $ catMaybes $ map (view delegatedPool) acc)
  
queryAllRegisteredPools :: ClientM [Pool]
queryAllRegisteredPools = do
  (Pools pools) <- queryPoolList
  fmap concat $ mapM (queryPoolInfo (Just "not.is.null") (Just "not.is.null") . Pools) $ 
    groupInto 1000 pools

queryAddressUTxOs :: [PaymentAddress] -> ClientM [AddressUTxO]
queryAddressUTxOs addrs = 
    addressUTxOsApi select "eq.false" Nothing $ ExtendedPaymentAddresses addrs
  where
    select =
      toText $ intercalate ","
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
queryAddressAssets addrs = addressAssetsApi select $ PaymentAddresses addrs
  where
    select =
      toText $ intercalate ","
        [ "policy_id"
        , "asset_name"
        , "fingerprint"
        , "quantity"
        ]

queryAddressTransactions :: [PaymentAddress] -> ClientM [Transaction]
queryAddressTransactions addrs =
    addressTxsApi "tx_hash" (PaymentAddresses addrs) >>= txInfoApi select
  where
    select = 
      toText $ intercalate ","
        [ "tx_hash"
        , "tx_timestamp"
        , "block_height"
        , "fee"
        , "tx_size"
        , "deposit"
        , "invalid_before"
        , "invalid_after"
        , "collateral_inputs"
        , "collateral_output"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        , "certificates"
        -- , "withdrawals"
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

queryUnknownUTxOs :: [TxOutRef] -> ClientM [AddressUTxO]
queryUnknownUTxOs refs = 
    utxoInfoApi select $ UnknownUTxOs refs
  where
    select =
      toText $ intercalate ","
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

queryStakeRewards :: [StakeAddress] -> ClientM [[StakeReward]]
queryStakeRewards addrs = stakeRewardsApi select $ StakeAddresses addrs
  where
    select =
      toText $ intercalate ","
        [ "rewards"
        ]

queryPoolList :: ClientM Pools
queryPoolList = do
    queryPools 0 (Pools [])
  where
    queryPools :: Int -> Pools -> ClientM Pools
    queryPools offset !acc = do
      !res@(Pools pools) <- poolListApi select offset "eq.registered"
      if length pools == 1000
      then queryPools (offset + 1000) (acc <> res)
      else return $ acc <> res

    select =
      toText $ intercalate ","
        [ "pool_id_bech32"
        , "pool_status"
        ]

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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- Break a list into sublists of the specified length.
groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = 
  take n xs : groupInto n (drop n xs)
