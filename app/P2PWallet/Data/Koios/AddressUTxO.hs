{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/address_utxos API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.AddressUTxO where

import Data.Aeson

import P2PWallet.Prelude
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Plutus

-- | The type representing the information returned with the address_utxos API endpoint.
data AddressUTxO = AddressUTxO
  { _paymentAddress :: PaymentAddress
  , _stakeAddress :: Maybe StakeAddress
  , _utxoRef :: TxOutRef
  , _lovelaces :: Lovelace
  , _datumHash :: Maybe Text
  , _inlineDatum :: Maybe Value
  , _referenceScriptHash :: Maybe Text
  , _nativeAssets :: [NativeAsset]
  , _blockTime :: POSIXTime
  , _blockHeight :: Integer
  } deriving (Show,Eq)

instance FromJSON AddressUTxO where
  parseJSON =
      withObject "AddressUTxO" $ \o ->
        AddressUTxO
          <$> o .: "address"
          <*> o .: "stake_address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . readTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "inline_datum" >>= 
                maybe (return Nothing) (withObject "inlineDatum" $ \i -> i .: "value"))
          <*> (o .: "reference_script" >>= 
                maybe (return Nothing) (withObject "referenceScript" $ \i -> i .: "hash"))
          <*> o .: "asset_list"
          <*> o .: "block_time"
          <*> o .: "block_height"
    where
      concatRef :: Text -> Integer -> Text
      concatRef hash idx = hash <> "#" <> show idx
