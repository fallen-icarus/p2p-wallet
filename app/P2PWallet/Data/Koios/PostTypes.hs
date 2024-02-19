{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Koios.PostTypes where

import Data.Aeson
import Data.Vector (Vector)

import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.PoolID
import P2PWallet.Data.Core.TxCBOR
import P2PWallet.Data.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Tx Submission
-------------------------------------------------
-- | A newtype for submitting a finalized transaction to Koios' ogmios endpoint for submission.
newtype SubmitTxCBOR = SubmitTxCBOR TxCBOR

instance ToJSON SubmitTxCBOR where
  toJSON (SubmitTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("submitTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Script Budget Evaluation
-------------------------------------------------
-- | A newtype for submitting a raw transaction to Koios' ogmios endpoint for script budget
-- evaluation.
newtype EvaluateTxCBOR = EvaluateTxCBOR TxCBOR

instance ToJSON EvaluateTxCBOR where
  toJSON (EvaluateTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("evaluateTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Payment Addresses
-------------------------------------------------
-- | A newtype for submitting a list of payment addresses.
newtype PaymentAddresses = PaymentAddresses [PaymentAddress] 
  deriving (Show)

instance ToJSON PaymentAddresses where
  toJSON (PaymentAddresses as) = 
    object [ "_addresses" .= map unPaymentAddress as 
           ]

-- | A newtype for submitting a list of payment addresses with the "_extended" flag.
newtype ExtendedPaymentAddresses = ExtendedPaymentAddresses [PaymentAddress] 
  deriving (Show)

instance ToJSON ExtendedPaymentAddresses where
  toJSON (ExtendedPaymentAddresses as) = 
    object [ "_addresses" .= map unPaymentAddress as 
           , "_extended" .= True
           ]

-------------------------------------------------
-- Stake Addresses
-------------------------------------------------
-- | A newtype for submitting a list of stake addresses.
newtype StakeAddresses = StakeAddresses [StakeAddress] 
  deriving (Show)

instance ToJSON StakeAddresses where
  toJSON (StakeAddresses as) = 
    object [ "_stake_addresses" .= map unStakeAddress as 
           ]

-------------------------------------------------
-- Transactions
-------------------------------------------------
-- | A newtype for submitting a list of transaction hashes. This is also the return type for
-- an intermediate query.
newtype TxHashes = TxHashes (Vector Text) deriving (Show)

instance ToJSON TxHashes where
  toJSON (TxHashes txs) = object [ "_tx_hashes" .= txs ]

instance FromJSON TxHashes where
  parseJSON = 
    withArray "TxHashes" $ fmap TxHashes . mapM (withObject "TxHash" (.: "tx_hash"))

-------------------------------------------------
-- Unknown UTxOs
-------------------------------------------------
-- | A newtype for submitting a list of unknown UTxOs to lookup.
newtype UnknownUTxOs = UnknownUTxOs [TxOutRef]
  deriving (Show)

instance ToJSON UnknownUTxOs where
  toJSON (UnknownUTxOs refs) = object [ "_utxo_refs" .= map (showTxOutRef @Text) refs ]

-------------------------------------------------
-- Pools
-------------------------------------------------
-- | A newtype for submitting a list of `PoolID`s. This is also the return type for an intermediate
-- query.
newtype Pools = Pools [PoolID] deriving (Show)

instance FromJSON Pools where
  parseJSON =
    withArray "Pools" $ 
      fmap (Pools . toList) . mapM (withObject "PoolList" (.: "pool_id_bech32"))

instance ToJSON Pools where
  toJSON (Pools pools) = object [ "_pool_bech32_ids" .= pools ]

instance Semigroup Pools where
  (Pools !p1) <> (Pools !p2) = Pools $ p1 <> p2
