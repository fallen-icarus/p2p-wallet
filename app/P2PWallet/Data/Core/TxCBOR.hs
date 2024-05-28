{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.TxCBOR where

import Data.Aeson

import P2PWallet.Prelude

-- | A type representing the cbor from a cardano-cli produced transaction.
newtype TxCBOR = TxCBOR { unTxCBOR :: Text }

makeFieldLabelsNoPrefix ''TxCBOR

instance FromJSON TxCBOR where
  parseJSON (Object o) = TxCBOR <$> o .: "cborHex"
  parseJSON _ = mzero

