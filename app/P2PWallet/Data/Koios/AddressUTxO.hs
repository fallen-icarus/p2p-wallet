{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/address_utxos API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.AddressUTxO where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | The type representing the information returned with the address_utxos API endpoint.
data AddressUTxO = AddressUTxO
  { paymentAddress :: PaymentAddress
  , stakeAddress :: Maybe StakeAddress
  , utxoRef :: TxOutRef
  , lovelace :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScript :: Maybe ReferenceScript
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AddressUTxO

instance FromJSON AddressUTxO where
  parseJSON =
      withObject "AddressUTxO" $ \o ->
        AddressUTxO
          <$> o .: "address"
          <*> o .: "stake_address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . parseTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "inline_datum" >>= 
                maybe (return Nothing) (withObject "inlineDatum" $ \i -> i .: "value"))
          <*> o .: "reference_script"
          <*> o .: "asset_list"
          <*> o .: "block_time"
          <*> o .: "block_height"
    where
      concatRef :: Text -> Integer -> Text
      concatRef hash idx = hash <> "#" <> show idx

-------------------------------------------------
-- Converting to another UTxO type
-------------------------------------------------
class FromAddressUTxO a where
  fromAddressUTxO :: AddressUTxO -> a
