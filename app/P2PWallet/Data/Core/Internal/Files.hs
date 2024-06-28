{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.Internal.Files
  ( 
    ParamsFile(..)
  , TxBodyFile(..)
  , TransformedTxFile(..)
  , KeyWitnessFile(..)
  , SignedTxFile(..)
  , HwSigningFile(..)
  , PubKeyFile(..)
  , CertificateFile(..)
  , TmpDirectory(..)
  ) where

import P2PWallet.Prelude

-- | A newtype wrapper around the parameter's `FilePath`.
newtype ParamsFile = ParamsFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around the tx body's `FilePath`.
newtype TxBodyFile = TxBodyFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around the transformed tx's `FilePath`. 
newtype TransformedTxFile = TransformedTxFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around a transaction witness file for a key.
newtype KeyWitnessFile = KeyWitnessFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around a signed tx file.
newtype SignedTxFile = SignedTxFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around the HwSigingFile's `FilePath`.
newtype HwSigningFile = HwSigningFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around the hw pubkey's `FilePath`.
newtype PubKeyFile = PubKeyFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around a certificate's `FilePath`.
newtype CertificateFile = CertificateFile FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)

-- | A newtype wrapper around the temporary directory's absolute path.
newtype TmpDirectory = TmpDirectory FilePath
  deriving (Show)
  deriving newtype (Eq,Ord,ToText,ToString,IsString)
