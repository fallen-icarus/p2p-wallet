{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.TxCBOR where

import Data.Aeson

import P2PWallet.Prelude

-- | A type representing the cbor from a cardano-cli produced transaction.
newtype TxCBOR = TxCBOR Text

instance FromJSON TxCBOR where
  parseJSON (Object o) = TxCBOR <$> o .: "cborHex"
  parseJSON _ = mzero

