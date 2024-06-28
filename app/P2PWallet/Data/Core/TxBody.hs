{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The intermediate representation for a transaction. The transaction builder model will be converted
to this representation before building. It is effectively a medium-level representation for a
transaction. It is assumed that the inputs and outputs are already balanced before converting to
`TxBody`.

-}
module P2PWallet.Data.Core.TxBody where

import Data.Aeson
import System.FilePath ((</>), (<.>))

import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Exporting Smart Contract Files
-------------------------------------------------
-- | A class for exporting the required smart contract files for use with cardano-cli.
class ExportContractFiles a where
  exportContractFiles :: a -> IO ()

-- | Export a redeemer for use with a smart contract. The file name is the hash of the 
-- redeemer `Data`.
exportRedeemer :: Redeemer -> IO ()
exportRedeemer redeemer = do
  tmpDir <- getTemporaryDirectory
  writeData (tmpDir </> show (hashRedeemer redeemer) <.> "json") redeemer

-------------------------------------------------
-- Execution Budgets
-------------------------------------------------
-- | The amount of memory and cpu steps required for a smart contract execution.
data ExecutionBudget = ExecutionBudget
  { memory :: Integer
  , cpu :: Integer
  } deriving (Show,Eq,Ord)

instance Default ExecutionBudget where
  def = ExecutionBudget
    { memory = 0
    , cpu = 0
    }

instance Display ExecutionBudget where
  display ExecutionBudget{..} = 
    -- It must be wrapped in double quotes.
    show $ show @Text (cpu,memory)

instance FromJSON ExecutionBudget where
  parseJSON = withObject "ExecutionBudget" $ \o ->
    ExecutionBudget
      <$> o .: "memory"
      <*> o .: "cpu"

-------------------------------------------------
-- Script Witness
-------------------------------------------------
-- | Whether the script will be executed as a reference script or a normal script.
data ScriptWitness
  -- | The script will be executed as a reference script.
  = ReferenceWitness TxOutRef
  -- | The script will be included in the transaction and executed normally.
  | NormalWitness SerialisedScript
  deriving (Show,Eq,Ord)

-- | Export a plutus contract. The file name is the hash of the script.
exportScriptWitness :: ScriptWitness -> IO ()
exportScriptWitness (ReferenceWitness _) = return ()
exportScriptWitness (NormalWitness script) = do
  tmpDir <- getTemporaryDirectory
  writeScript (tmpDir </> show (hashScript script) <.> "plutus") script >>=
    either (throwIO . AppError . show) return

-------------------------------------------------
-- Inputs
-------------------------------------------------
-- | Information for a particular input.
newtype TxBodyInput = TxBodyInput
  -- | The input's output reference.
  { utxoRef :: TxOutRef
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''TxBodyInput

-------------------------------------------------
-- Outputs
-------------------------------------------------
-- | Information for a particular output.
data TxBodyOutput = TxBodyOutput
  -- | The target address.
  { paymentAddress :: PaymentAddress
  -- | The amount of lovelace to deposit for this output.
  , lovelace :: Lovelace
  -- | The native assets to store in this output.
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBodyOutput

-------------------------------------------------
-- Certificates
-------------------------------------------------
-- | Types of certificate. When registering and delegating in the same transaction,
-- the registration certificate MUST be first.
data CertificateAction
  -- | Register a staking credential and pay the 2 ADA deposit.
  = Registration
  -- | Deregister a staking credential and recover the 2 ADA deposit.
  | Deregistration
  -- | Delegate the staking credential.
  | Delegation PoolID
  deriving (Show,Eq,Ord)

makePrisms ''CertificateAction

instance Display CertificateAction where
  display Registration = "Stake Registration"
  display Deregistration = "Stake Deregistration"
  display (Delegation (PoolID poolId)) = "Delegation to " <> toText poolId

-- | The type representing user supplied information for a certificate.
data TxBodyCertificate = TxBodyCertificate
  -- | The stake credential used for the stake address.
  { stakeCredential :: Credential
  -- | What kind of certificate this is.
  , certificateAction :: CertificateAction
  -- | The bech32 stake address. 
  , stakeAddress :: StakeAddress
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''TxBodyCertificate

-------------------------------------------------
-- Withdrawals
-------------------------------------------------
-- | Information for a particular withdrawal.
data TxBodyWithdrawal = TxBodyWithdrawal
  -- | The staking credential. This is used for sorting.
  { stakeCredential :: Credential
  -- | The target address.
  , stakeAddress :: StakeAddress
  -- | The amount of lovelace withdrawn from the rewards address.
  , lovelace :: Lovelace
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''TxBodyWithdrawal

-------------------------------------------------
-- Mint/Burn
-------------------------------------------------
-- | Information for a particular mint/burn.
data TxBodyMint = TxBodyMint
  -- | The minting policy hash.
  { mintingPolicyHash :: ScriptHash
  -- | The tokens to mint/burn.
  , nativeAssets :: [NativeAsset]
  -- | The redeemer to use.
  , redeemer :: Redeemer
  -- | The minting script witness.
  , scriptWitness :: ScriptWitness
  -- | The estimated execution budget.
  , executionBudget :: ExecutionBudget
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''TxBodyMint

instance ExportContractFiles TxBodyMint where
  exportContractFiles TxBodyMint{redeemer,scriptWitness} = do
    -- | Export the minting policy.
    exportScriptWitness scriptWitness

    -- | Export the redeemer file.
    exportRedeemer redeemer

-------------------------------------------------
-- Collateral
-------------------------------------------------
-- | Information for a collateral input. Collateral is hard-coded to provide 4 ADA as collateral
-- in the case the collateral is actually required. Collateral inputs must contain at least 5
-- ADA so that the change is contains a valid amount of ADA (1 ADA in this case). Collateral
-- should never actually need to be used.
data TxBodyCollateral = TxBodyCollateral
  -- | The input's output reference.
  { utxoRef :: TxOutRef
  -- | The total amount in the collateral UTxO. It must be at least 5 ADA so that 4 ADA can be
  -- used if necessary and 1 ADA can be returned.
  , lovelace :: Lovelace
  -- | The address where the collateral input is from. The collateral change will be returned to
  -- this address.
  , paymentAddress :: PaymentAddress
  } deriving (Show,Eq)

-------------------------------------------------
-- Tx Body
-------------------------------------------------
data TxBody = TxBody
  { inputs :: [TxBodyInput]
  , outputs :: [TxBodyOutput]
  , certificates :: [TxBodyCertificate]
  , withdrawals :: [TxBodyWithdrawal]
  , mints :: [TxBodyMint]
  , collateralInput :: Maybe TxBodyCollateral -- Only one collateral input is necessary.
  , keyWitnesses :: [KeyWitness]
  , fee :: Lovelace
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBody

instance Semigroup TxBody where
  txBody1 <> txBody2 = TxBody
    { inputs = 
        -- Add the new inputs and sort lexicographically. This makes estimated execution budgets
        -- easier.
        sort $ txBody1 ^. #inputs <> txBody2 ^. #inputs
    , outputs = 
        -- Add the new outputs while preserving ordering.
        txBody1 ^. #outputs <> txBody2 ^. #outputs
    , certificates = 
        -- The registration certificate must appear before the delegation certificate for a given
        -- stake address. The certificates should be sorted by credential to make getting budget
        -- estimations easier.
        sort $ txBody1 ^. #certificates <> txBody2 ^. #certificates
    , withdrawals = 
        -- Sort the withdrawals by credential. This makes getting the execution budgets easier.
        sort $ txBody1 ^. #withdrawals <> txBody2 ^. #withdrawals
    , mints = 
        -- Sort them by policy id. This makes getting the execution budgets easier.
        -- Redeemer and witness collisions must be caught before converting to `TxBody`!
        sort $ txBody1 ^. #mints <> txBody2 ^. #mints
    , collateralInput = 
       -- There should only be one txBody with collateral set.
        txBody1 ^. #collateralInput <|> txBody2 ^. #collateralInput
    , keyWitnesses = 
        -- Remove duplicates.
        ordNub $ txBody1 ^. #keyWitnesses <> txBody2 ^. #keyWitnesses
    , fee = 
        -- Most txBodies will have a fee of zero so the non-zero one must be chosen.
        -- I do not think there is a case where two txBodies will both have fees set.
        max (txBody1 ^. #fee) (txBody2 ^. #fee)
    }

instance Monoid TxBody where
  mempty = TxBody
    { inputs = []
    , outputs = []
    , certificates = []
    , withdrawals = []
    , mints = []
    , collateralInput = Nothing
    , keyWitnesses = []
    , fee = 0
    }

instance ExportContractFiles TxBody where
  exportContractFiles TxBody{..} = do
    -- Export all files for the necessary minting policies.
    mapM_ exportContractFiles mints

class AddToTxBody a where
  -- Some highlevel actions impact multiple parts of the transaction.
  addToTxBody :: TxBody -> a -> TxBody
