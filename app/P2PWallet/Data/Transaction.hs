{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

In order to store the transactions in the sqlite database, the Koios version must be converted to
a new form. Technically, newtype wrappers could work, but I would rather only do the conversions
once. Wrappers would required "unwrapping" with every use.

-}
module P2PWallet.Data.Transaction where

import Data.Aeson

import Database.SQLite.Simple (field,ToRow(..),FromRow(..))
import Database.SQLite.Simple.ToField (toField)

import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Data.Koios.Transaction qualified as Koios
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Transaction UTxO
-------------------------------------------------
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
-- Transaction
-------------------------------------------------
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
  -- , collateralOutput :: Maybe TransactionUTxO
  -- , showCollateralOutput :: Bool
  , referenceInputs :: [TransactionUTxO]
  , showReferenceInputs :: Bool
  , inputs :: [TransactionUTxO]
  , showInputs :: Bool
  , outputs :: [TransactionUTxO]
  , showOutputs :: Bool
  -- , certificates :: [TransactionCertificate]
  -- , withdrawals :: [TransactionWithdrawal]
  -- , nativeAssetsMinted :: Value
  -- , nativeScripts :: Value
  -- , plutusContracts :: Value
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
    -- collateralOutput <- fromMaybe mzero . decode <$> field
    referenceInputs <- fromMaybe mzero . decode <$> field
    inputs <- fromMaybe mzero . decode <$> field
    outputs <- fromMaybe mzero . decode <$> field
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
      -- , collateralOutput = collateralOutput
      , referenceInputs = referenceInputs
      , inputs = inputs
      , outputs = outputs
      , showCollateralInputs = False
      -- , showCollateralOutput = False
      , showReferenceInputs = False
      , showInputs = False
      , showOutputs = False
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
    -- , toField $ encode collateralOutput
    , toField $ encode referenceInputs
    , toField $ encode inputs
    , toField $ encode outputs
    ]

instance TableName Transaction where
  tableName = "transactions"

instance Creatable Transaction where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @Transaction
    , "("
    , unwords $ intersperse ","
        [ "tx_hash TEXT PRIMARY KEY"
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
        -- , "collateral_output BLOB"
        , "reference_inputs BLOB"
        , "inputs BLOB"
        , "outputs BLOB"
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
        -- , "collateral_output"
        , "reference_inputs"
        , "inputs"
        , "outputs"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?);"
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
  -- , collateralOutput = fmap toTransactionUTxO collateralOutput
  , referenceInputs = map toTransactionUTxO referenceInputs
  , inputs = map toTransactionUTxO inputs
  , outputs = map toTransactionUTxO outputs
  , showCollateralInputs = False
  -- , showCollateralOutput = False
  , showReferenceInputs = False
  , showInputs = False
  , showOutputs = False
  }

-- collateralOutputLens :: Lens' Transaction [TransactionUTxO]
-- collateralOutputLens = lens getOutput setOutput
--   where
--     getOutput :: Transaction -> [TransactionUTxO]
--     getOutput = maybe [] (:[]) . view #collateralOutput
--
--     setOutput :: Transaction -> [TransactionUTxO] -> Transaction
--     setOutput tx output = tx & #collateralOutput .~ maybeHead output

