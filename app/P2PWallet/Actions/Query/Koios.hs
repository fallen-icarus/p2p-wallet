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
  ) where

import Servant.Client (client, ClientM, runClientM, Scheme(Https), BaseUrl(..), mkClientEnv)
import Servant.Client qualified as Client
import Servant.API ((:<|>)(..), JSON, Post, Get, (:>), ReqBody, Required, QueryParam', QueryParam)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson 
import Data.List qualified as List

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.PostTypes
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Transaction qualified as P2P
import P2PWallet.Data.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- High-Level API
-------------------------------------------------
toNetworkURL :: Network -> String
toNetworkURL Mainnet = "api.koios.rest"
toNetworkURL Testnet = "preprod.koios.rest"

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

submitApi
  :<|> evaluateApi 
  :<|> addressUTxOsApi 
  :<|> addressTxsApi
  :<|> addressAssetsApi 
  :<|> txInfoApi 
  = client (Proxy :: Proxy KoiosApi)

queryPaymentWalletInfo 
  :: PaymentAddress 
  -> Integer 
  -> ClientM ([AddressUTxO],[Transaction],[NativeAsset])
queryPaymentWalletInfo addr lastBlock = 
  (,,) <$> queryAddressUTxOs [addr]
       <*> queryAddressTransactions [addr] lastBlock
       <*> queryAddressAssets [addr]

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
        , "collateral_output"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        , "certificates"
        , "withdrawals"
        -- , "assets_minted"
        -- , "native_scripts"
        -- , "plutus_contracts" 
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- Break a list into sublists of the specified length.
groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = 
  take n xs : groupInto n (drop n xs)
