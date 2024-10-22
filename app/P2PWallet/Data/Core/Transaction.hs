{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

In order to store the transactions in the sqlite database, the Koios version must be converted to
a new form. 

-}
module P2PWallet.Data.Core.Transaction 
  ( -- * UTxOs
    TransactionUTxO(..)
  , toTransactionUTxO

    -- * Certificates
  , Koios.TransactionCertificate(..)
  , Koios.CertificateType(..)
  , Koios.CertificateInfo(..)
  , Koios._StakeDelegationInfo
  , Koios._StakeRegistrationInfo
  , Koios._VoteDelegationInfo
  , Koios._OtherInfo

    -- * Withdrawals
  , Koios.TransactionWithdrawal(..)

    -- * Plutus Contracts
  , TransactionPlutusContract(..)
  , Koios.ContractPurpose(..)

   -- * Transactions
  , Transaction(..)
  , toTransaction
  ) where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.Transaction qualified as Koios
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Transaction UTxO
-------------------------------------------------
-- | A custom UTxO type since the Koios version uses different json encoding.
data TransactionUTxO = TransactionUTxO
  { paymentAddress :: PaymentAddress
  , stakeAddress :: Maybe StakeAddress
  , utxoRef :: TxOutRef
  , lovelace :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  , showDetails :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionUTxO

instance Default TransactionUTxO where
  def = TransactionUTxO
    { paymentAddress = ""
    , stakeAddress = Nothing
    , utxoRef = TxOutRef "" 0
    , lovelace = 0
    , datumHash = Nothing
    , inlineDatum = Nothing
    , referenceScriptHash = Nothing
    , nativeAssets = []
    , showDetails = False
    }

instance ToJSON TransactionUTxO where
  toJSON TransactionUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "lovelace" .= lovelace
           , "datum_hash" .= datumHash
           , "inline_datum" .= inlineDatum
           , "reference_script_hash" .= referenceScriptHash
           , "native_assets" .= nativeAssets
           , "payment_address" .= paymentAddress
           , "stake_address" .= stakeAddress
           ]

instance FromJSON TransactionUTxO where
  parseJSON = withObject "TransactionUTxO" $ \o ->
    TransactionUTxO
      <$> o .: "payment_address"
      <*> o .: "stake_address"
      <*> o .: "utxo_ref"
      <*> o .: "lovelace"
      <*> o .: "datum_hash"
      <*> o .: "inline_datum"
      <*> o .: "reference_script_hash"
      <*> o .: "native_assets"
      <*> return False

toTransactionUTxO :: Koios.TransactionUTxO -> TransactionUTxO
toTransactionUTxO Koios.TransactionUTxO{..} = TransactionUTxO
  { paymentAddress = paymentAddress
  , stakeAddress = stakeAddress
  , utxoRef = utxoRef
  , lovelace = lovelace
  , datumHash = datumHash
  , inlineDatum = inlineDatum
  , referenceScriptHash = referenceScriptHash
  , nativeAssets = nativeAssets
  , showDetails = False
  }

-------------------------------------------------
-- TransactionPlutusContract
-------------------------------------------------
-- | A custom PlutusContract type since the Koios version uses different json encoding.
data TransactionPlutusContract = TransactionPlutusContract
  { paymentAddress :: Maybe PaymentAddress
  , spendsInput :: Maybe TxOutRef
  , scriptHash :: Text
  , datum :: Maybe Value
  , redeemer :: Value
  , purpose :: Koios.ContractPurpose
  , showDetails :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionPlutusContract

instance ToJSON TransactionPlutusContract where
  toJSON TransactionPlutusContract{..} =
    object [ "payment_address" .= paymentAddress
           , "spends_input" .= spendsInput
           , "script_hash" .= scriptHash
           , "datum" .= datum
           , "redeemer" .= redeemer
           , "purpose" .= purpose
           ]

instance FromJSON TransactionPlutusContract where
  parseJSON = withObject "TransactionPlutusContract" $ \o ->
    TransactionPlutusContract
      <$> o .: "payment_address"
      <*> o .: "spends_input"
      <*> o .: "script_hash"
      <*> o .: "datum"
      <*> o .: "redeemer"
      <*> o .: "purpose"
      <*> pure False

toTransactionPlutusContract :: Koios.TransactionPlutusContract -> TransactionPlutusContract
toTransactionPlutusContract Koios.TransactionPlutusContract{..} = TransactionPlutusContract
  { paymentAddress = paymentAddress
  , spendsInput = spendsInput
  , scriptHash = scriptHash
  , datum = datum
  , redeemer = redeemer
  , purpose = purpose
  , showDetails = False
  }

-------------------------------------------------
-- Transaction
-------------------------------------------------
-- | The transaction type used by the GUI.
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
  , showCollateralInputs :: Bool
  , referenceInputs :: [TransactionUTxO]
  , showReferenceInputs :: Bool
  , inputs :: [TransactionUTxO]
  , showInputs :: Bool
  , outputs :: [TransactionUTxO]
  , showOutputs :: Bool
  , certificates :: [Koios.TransactionCertificate]
  , showCertificates :: Bool
  , withdrawals :: [Koios.TransactionWithdrawal]
  , showWithdrawals :: Bool
  , mints :: [NativeAsset]
  , showMints :: Bool
  , plutusContracts :: [TransactionPlutusContract]
  , showPlutusContracts :: Bool
  } deriving (Generic,ToJSON,FromJSON,Show,Eq)

makeFieldLabelsNoPrefix ''Transaction

instance Default Transaction where
  def = Transaction
    { txHash = ""
    , blockTime = 0
    , blockHeight = 0
    , fee = 0
    , size = 0
    , deposit = 0
    , invalidBefore = Nothing
    , invalidAfter = Nothing
    , collateralInputs = []
    , showCollateralInputs = False
    , referenceInputs = []
    , showReferenceInputs = False
    , inputs = []
    , showInputs = False
    , outputs = []
    , showOutputs = False
    , certificates = []
    , showCertificates = False
    , withdrawals = []
    , showWithdrawals = False
    , mints = []
    , showMints = False
    , plutusContracts = []
    , showPlutusContracts = False
    }

-- | Convert a Koios Transaction to a P2PWallet Transaction.
toTransaction :: Koios.Transaction -> Transaction
toTransaction Koios.Transaction{..} = Transaction
  { txHash = txHash
  , blockTime = blockTime
  , blockHeight = blockHeight
  , fee = fee
  , size = size
  , deposit = deposit
  , invalidBefore = invalidBefore
  , invalidAfter = invalidAfter
  , collateralInputs = map toTransactionUTxO collateralInputs
  , referenceInputs = map toTransactionUTxO referenceInputs
  , inputs = map toTransactionUTxO inputs
  , outputs = map toTransactionUTxO outputs
  , certificates = certificates
  , withdrawals = withdrawals
  , mints = mints
  , plutusContracts = map toTransactionPlutusContract plutusContracts
  , showCollateralInputs = False
  , showReferenceInputs = False
  , showInputs = False
  , showOutputs = False
  , showCertificates = False
  , showWithdrawals = False
  , showMints = False
  , showPlutusContracts = False
  }
