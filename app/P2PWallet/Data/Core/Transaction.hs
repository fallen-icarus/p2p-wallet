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
  , Koios._DelegationInfo
  , Koios._StakeRegistrationInfo
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

import Database.SQLite.Simple (field,ToRow(..),FromRow(..))
import Database.SQLite.Simple.ToField (toField)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.Transaction qualified as Koios
import P2PWallet.Database
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
  , profileId :: ProfileId
  , paymentId :: PaymentId
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
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Transaction

instance FromRow Transaction where
  fromRow = do
    txHash <- field
    profileId <- field
    paymentId <- field
    blockTime <- field
    blockHeight <- field
    fee <- field
    size <- field
    deposit <- field
    invalidBefore <- field
    invalidAfter <- field
    collateralInputs <- fromMaybe mzero . decode <$> field
    referenceInputs <- fromMaybe mzero . decode <$> field
    inputs <- fromMaybe mzero . decode <$> field
    outputs <- fromMaybe mzero . decode <$> field
    certificates <- fromMaybe mzero . decode <$> field
    withdrawals <- fromMaybe mzero . decode <$> field
    mints <- fromMaybe mzero . decode <$> field
    plutusContracts <- fromMaybe mzero . decode <$> field
    return $ Transaction
      { txHash = txHash
      , profileId = profileId
      , paymentId = paymentId
      , blockTime = blockTime
      , blockHeight = blockHeight
      , fee = fee
      , size = size
      , deposit = deposit
      , invalidBefore = invalidBefore
      , invalidAfter = invalidAfter
      , collateralInputs = collateralInputs
      , referenceInputs = referenceInputs
      , inputs = inputs
      , outputs = outputs
      , certificates = certificates
      , withdrawals = withdrawals
      , mints = mints
      , plutusContracts = plutusContracts
      , showCollateralInputs = False
      , showReferenceInputs = False
      , showInputs = False
      , showOutputs = False
      , showCertificates = False
      , showWithdrawals = False
      , showMints = False
      , showPlutusContracts = False
      }

instance ToRow Transaction where
  toRow Transaction{..} =
    [ toField txHash
    , toField profileId
    , toField paymentId
    , toField blockTime
    , toField blockHeight
    , toField fee
    , toField size
    , toField deposit
    , toField invalidBefore
    , toField invalidAfter
    , toField $ encode collateralInputs
    , toField $ encode referenceInputs
    , toField $ encode inputs
    , toField $ encode outputs
    , toField $ encode certificates
    , toField $ encode withdrawals
    , toField $ encode mints
    , toField $ encode plutusContracts
    ]

instance TableName Transaction where
  tableName = "transactions"

instance Creatable Transaction where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @Transaction
    , "("
    , unwords $ intersperse ","
        [ "tx_hash TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "payment_id INTEGER REFERENCES payment_wallets (payment_id)"
        , "block_time TEXT NOT NULL"
        , "block_height INTEGER NOT NULL"
        , "fee INTEGER NOT NULL"
        , "size INTEGER NOT NULL"
        , "deposit INTEGER NOT NULL"
        , "invalid_before TEXT"
        , "invalid_after TEXT"
        , "collateral_inputs BLOB"
        , "reference_inputs BLOB"
        , "inputs BLOB"
        , "outputs BLOB"
        , "certificates BLOB"
        , "withdrawals BLOB"
        , "mints BLOB"
        , "plutus_contracts BLOB"
        , "PRIMARY KEY (tx_hash,payment_id)"
        ]
    , ");"
    ]

instance Insertable Transaction where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @Transaction
    , "("
    , unwords $ intersperse ","
        [ "tx_hash"
        , "profile_id"
        , "payment_id"
        , "block_time"
        , "block_height"
        , "fee"
        , "size"
        , "deposit"
        , "invalid_before"
        , "invalid_after"
        , "collateral_inputs"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        , "certificates"
        , "withdrawals"
        , "mints"
        , "plutus_contracts"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

-- | Convert a Koios Transaction to a P2PWallet Transaction.
toTransaction :: ProfileId -> PaymentId -> Koios.Transaction -> Transaction
toTransaction profileId paymentId Koios.Transaction{..} = Transaction
  { txHash = txHash
  , profileId = profileId
  , paymentId = paymentId
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
