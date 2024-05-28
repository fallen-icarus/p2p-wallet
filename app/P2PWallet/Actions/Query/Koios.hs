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

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.PostTypes
import P2PWallet.Data.Koios.Transaction
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
runQueryPaymentWalletInfo network wallet = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager (BaseUrl Https (toNetworkURL network) 443 "api/v1")
  runClientM (queryPaymentWalletInfo $ wallet ^. #paymentAddress) env >>= \case
    Right (us,_,_) -> 
      return $ Right $ 
        wallet & #utxos .~ map toPersonalUTxO us
               & #lovelace .~ sum (map (view #lovelace) us)
               -- & #txHistory .~ txs
               -- & #nativeAssets .~ as
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

submitApi
  :<|> evaluateApi 
  :<|> addressUTxOsApi 
  :<|> addressTxsApi
  :<|> addressAssetsApi 
  :<|> txInfoApi 
  = client (Proxy :: Proxy KoiosApi)

queryPaymentWalletInfo :: PaymentAddress -> ClientM ([AddressUTxO],[Transaction],[NativeAsset])
queryPaymentWalletInfo addr = 
  (,,) <$> queryAddressUTxOs [addr]
       <*> return [] -- queryAddressTransactions [addr]
       <*> queryAddressAssets [addr]

queryAddressUTxOs :: [PaymentAddress] -> ClientM [AddressUTxO]
queryAddressUTxOs addrs = 
    addressUTxOsApi select "eq.false" Nothing $ ExtendedPaymentAddresses addrs
  where
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
queryAddressAssets addrs = addressAssetsApi select $ PaymentAddresses addrs
  where
    select =
      mconcat $ intersperse ","
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
