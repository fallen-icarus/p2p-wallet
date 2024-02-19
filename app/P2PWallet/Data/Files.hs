{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Files
  ( 
    ParamsFile(..)
  , TxBodyFile(..)
  , TransformedTxFile(..)
  , WitnessFile(..)
  , SignedTxFile(..)
  , HwSigningFile(..)
  , PubKeyFile(..)
  , CertificateFile(..)
  ) where

import Data.Text qualified as T

import P2PWallet.Prelude

-- | A newtype wrapper around the parameter's `FilePath`.
newtype ParamsFile = ParamsFile FilePath

instance ToText ParamsFile where
  toText (ParamsFile file) = T.pack file

instance ToString ParamsFile where
  toString (ParamsFile file) = file

-- | A newtype wrapper around the tx body's `FilePath`.
newtype TxBodyFile = TxBodyFile FilePath

instance ToText TxBodyFile where
  toText (TxBodyFile file) = T.pack file

instance ToString TxBodyFile where
  toString (TxBodyFile file) = file

-- | A newtype wrapper around the transformed tx's `FilePath`. 
newtype TransformedTxFile = TransformedTxFile FilePath

instance ToText TransformedTxFile where
  toText (TransformedTxFile file) = T.pack file

instance ToString TransformedTxFile where
  toString (TransformedTxFile file) = file

-- | A newtype wrapper around a transaction witness file.
newtype WitnessFile = WitnessFile FilePath

instance ToText WitnessFile where
  toText (WitnessFile file) = T.pack file

instance ToString WitnessFile where
  toString (WitnessFile file) = file

-- | A newtype wrapper around a signed tx file.
newtype SignedTxFile = SignedTxFile FilePath

instance ToText SignedTxFile where
  toText (SignedTxFile file) = T.pack file

instance ToString SignedTxFile where
  toString (SignedTxFile file) = file

-- | A newtype wrapper around the HwSigingFile's `FilePath`.
newtype HwSigningFile = HwSigningFile FilePath
  deriving (Eq)

instance ToText HwSigningFile where
  toText (HwSigningFile file) = T.pack file

instance ToString HwSigningFile where
  toString (HwSigningFile file) = file

-- | A newtype wrapper around the hw pubkey's `FilePath`.
newtype PubKeyFile = PubKeyFile FilePath

instance ToText PubKeyFile where
  toText (PubKeyFile file) = T.pack file

instance ToString PubKeyFile where
  toString (PubKeyFile file) = file

-- | A newtype wrapper around a certificate's `FilePath`.
newtype CertificateFile = CertificateFile FilePath

instance ToText CertificateFile where
  toText (CertificateFile file) = T.pack file

instance ToString CertificateFile where
  toString (CertificateFile file) = file
