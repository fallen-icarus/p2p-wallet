{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/tx_info API endpoint 
(the types are the same for mainnet). The UTxOs returned with this endpoint are uniquely structured
which is why there is a dedicated `TransactionUTxO` type.

-}
module P2PWallet.Data.Koios.Transaction where

import Data.Aeson

import P2PWallet.Prelude
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Plutus

-------------------------------------------------
-- TransactionUTxO
-------------------------------------------------
-- | The type respesenting the UTxOs returned as part of the tx_info API endpoint.
data TransactionUTxO = TransactionUTxO
  { paymentAddress :: PaymentAddress
  , stakeAddress :: Maybe StakeAddress
  , utxoRef :: TxOutRef
  , lovelace :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionUTxO

instance FromJSON TransactionUTxO where
  parseJSON =
      withObject "TransactionUTxO" $ \o ->
        TransactionUTxO
          <$> (o .: "payment_addr" >>= withObject "payment_addr" (.: "bech32"))
          <*> o .: "stake_addr"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . parseTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "inline_datum" >>= 
                maybe (return Nothing) (withObject "inline_datum" $ \i -> i .: "value"))
          <*> (o .: "reference_script" >>= 
                maybe (return Nothing) (withObject "reference_script" $ \i -> i .: "hash"))
          <*> o .: "asset_list"
    where
      concatRef :: Text -> Integer -> Text
      concatRef hash idx = hash <> "#" <> show idx

-------------------------------------------------
-- Transaction
-------------------------------------------------
-- | The type respesenting the overall information returned with the tx_info query.
data Transaction = Transaction
  { txHash :: Text
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  , fee :: Lovelace
  , size :: Integer
  , deposit :: Lovelace
  , invalidBefore :: Maybe Text
  , invalidAfter :: Maybe Text
  , collateralInputs :: [TransactionUTxO]
  , referenceInputs :: [TransactionUTxO]
  , inputs :: [TransactionUTxO]
  , outputs :: [TransactionUTxO]
  -- , certificates :: [TransactionCertificate]
  -- , withdrawals :: [TransactionWithdrawal]
  -- , nativeAssetsMinted :: Value
  -- , nativeScripts :: Value
  -- , plutusContracts :: Value
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Transaction

instance FromJSON Transaction where
  parseJSON =
    withObject "Transaction" $ \o ->
      Transaction
        <$> o .: "tx_hash"
        <*> o .: "tx_timestamp"
        <*> o .: "block_height"
        <*> o .: "fee"
        <*> o .: "tx_size"
        <*> o .: "deposit"
        <*> o .: "invalid_before"
        <*> o .: "invalid_after"
        <*> o .: "collateral_inputs"
        <*> o .: "reference_inputs"
        <*> o .: "inputs"
        <*> o .: "outputs"
        -- <*> o .: "certificates"
        -- <*> o .: "withdrawals"
        -- <*> o .: "assets_minted"
        -- <*> o .: "native_scripts"
        -- <*> o .: "plutus_contracts"
