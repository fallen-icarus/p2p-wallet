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
import Data.Map.Strict qualified as Map

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

-- | Export a datum for use with a smart contract. The file name is the hash of the 
-- redeemer `Data`.
exportDatum :: Datum -> IO ()
exportDatum datum = do
  tmpDir <- getTemporaryDirectory
  writeData (tmpDir </> show (hashDatum datum) <.> "json") datum

-------------------------------------------------
-- Creating cardano-cli fields
-------------------------------------------------
class ToBuildCmdField a where
  -- | Add something to a cardano-cli command. The temporary directory is needed whenever
  -- files are required.
  toBuildCmdField :: TmpDirectory -> a -> Text

-- | Format the bash command so that it is more human-readable.
prettyFormatCmd :: Text -> Text
prettyFormatCmd = replace "--" "\\\n  --"

plutusFilePath :: TmpDirectory -> ScriptHash -> FilePath
plutusFilePath tmpDir scriptHash = toString tmpDir </> show scriptHash <.> "plutus"

redeemerFilePath :: TmpDirectory -> RedeemerHash -> FilePath
redeemerFilePath tmpDir redeemerHash = toString tmpDir </> show redeemerHash <.> "json"

datumFilePath :: TmpDirectory -> DatumHash -> FilePath
datumFilePath tmpDir datumHash = toString tmpDir </> show datumHash <.> "json"

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

instance ToJSON ExecutionBudget where
  toJSON ExecutionBudget{..} = object
    [ "memory" .= memory
    , "cpu" .= cpu
    ]
  
-------------------------------------------------
-- Script Witness
-------------------------------------------------
-- | Whether the script will be executed as a reference script or a normal script.
data ScriptWitness
  -- | The script will be executed as a reference script. The integer is its script size
  -- in bytes.
  = ReferenceWitness (TxOutRef, Integer)
  -- | The script will be included in the transaction and executed normally.
  | NormalWitness SerialisedScript
  deriving (Show,Eq,Ord)

makePrisms ''ScriptWitness

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
-- | The datum attached to an input.
data InputDatum
  -- | The input does not have a datum.
  = NoInputDatum
  -- | The input has an inline datum.
  | InputDatum
  -- | The input has a datum hash. The datum corresponds to the hash in the input.
  | InputDatumHash Datum
  deriving (Show,Eq)

makePrisms ''InputDatum

-- | Information for a spending script.
data SpendingScriptInfo = SpendingScriptInfo
  -- | The hash of the spending script. This is useful for doing second passes over the
  -- `TxBody`.
  { scriptHash :: ScriptHash
  -- | The spending script witness.
  , scriptWitness :: ScriptWitness
  -- | The datum attached to the input. The datum is needed when hashes are used.
  , datum :: InputDatum
  -- | The redeemer to use.
  , redeemer :: Redeemer
  -- | The estimated execution budget for this spending execution.
  , executionBudget :: ExecutionBudget
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SpendingScriptInfo

instance ToBuildCmdField SpendingScriptInfo where
  toBuildCmdField tmpDir SpendingScriptInfo{..} = case scriptWitness of
    ReferenceWitness (ref, _) -> unwords $ filter (/= "")
      [ "--spending-tx-in-reference " <> display ref
      , "--spending-plutus-script-v2"
      , unwords 
          [ "--spending-reference-tx-in-redeemer-file"
          , toText $ redeemerFilePath tmpDir $ hashRedeemer redeemer
          ]
      , "--spending-reference-tx-in-execution-units " <> display executionBudget
      , case datum of
          NoInputDatum -> ""
          InputDatum -> "--spending-reference-tx-in-inline-datum-present"
          InputDatumHash d -> unwords
            [ "--spending-reference-tx-in-datum-file"
            , toText $ datumFilePath tmpDir $ hashDatum d
            ]
      ]
    NormalWitness script -> unwords $ filter (/= "")
      [ "--tx-in-script-file " <> toText (plutusFilePath tmpDir $ hashScript script)
      , "--tx-in-redeemer-file " <> toText (redeemerFilePath tmpDir $ hashRedeemer redeemer)
      , "--tx-in-execution-units " <> display executionBudget
      , case datum of
          NoInputDatum -> ""
          InputDatum -> "--tx-in-inline-datum-present"
          InputDatumHash d -> "--tx-in-datum-file " <> toText (datumFilePath tmpDir $ hashDatum d)
      ]

-- | Information for a particular input.
data TxBodyInput = TxBodyInput
  -- | The input's output reference.
  { utxoRef :: TxOutRef
  -- | The information for the spending script required to spend this input.
  , spendingScriptInfo :: Maybe SpendingScriptInfo
  } deriving (Show,Eq)

instance Ord TxBodyInput where
  TxBodyInput{utxoRef=ref1} <= TxBodyInput{utxoRef=ref2} = ref1 <= ref2

makeFieldLabelsNoPrefix ''TxBodyInput

instance ExportContractFiles TxBodyInput where
  exportContractFiles TxBodyInput{spendingScriptInfo} =
    whenJust spendingScriptInfo $ \SpendingScriptInfo{..} -> do
      -- | Export the spending script.
      exportScriptWitness scriptWitness

      -- | Export the redeemer file.
      exportRedeemer redeemer

      -- | Export the datum file if the input is using a datum hash.
      whenJust (preview _InputDatumHash datum) exportDatum

instance ToBuildCmdField TxBodyInput where
  toBuildCmdField tmpDir TxBodyInput{..} = unwords $ filter (/= "")
    [ "--tx-in " <> display utxoRef
    , maybe "" (toBuildCmdField tmpDir) spendingScriptInfo
    ]

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
  -- | The datum to attach to the output.
  , datum :: OutputDatum
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBodyOutput

instance ExportContractFiles TxBodyOutput where
  exportContractFiles TxBodyOutput{datum} = do
    -- | Export the datum file if being used for an inline datum.
    whenJust (preview _OutputDatum datum) exportDatum

instance ToBuildCmdField TxBodyOutput where
  toBuildCmdField tmpDir TxBodyOutput{..} = unwords $ filter (/= "")
      [ "--tx-out"
      -- The output amount surrounded by quotes.
      , show $ unwords
          [ toText paymentAddress
          , show (unLovelace lovelace) <> " lovelace"
          , unwords $ for nativeAssets $ \NativeAsset{..} ->
              "+ " <> show quantity <> " " <> display policyId <> "." <> display tokenName
          ] 
      , outputDatumField datum
      ]
    where
      outputDatumField :: OutputDatum -> Text
      outputDatumField = toText . \case
        NoOutputDatum -> ""
        OutputDatumHash ds -> "--tx-out-datum-hash " <> show ds
        OutputDatum d -> "--tx-out-inline-datum-file " <> datumFilePath tmpDir (hashDatum d)

-------------------------------------------------
-- Certificates
-------------------------------------------------
-- | The type of vote delegation.
data VoteDelegation
  -- | The bech32 encoded DRep and whether it is a script.
  = DRepDelegation DRepID Bool
  | AlwaysAbstainDelegation
  | AlwaysNoDelegation
  deriving (Show,Eq,Ord)

makePrisms ''VoteDelegation

instance Display VoteDelegation where
  display (DRepDelegation drepId _) = "Vote delegation to " <> display drepId
  display AlwaysAbstainDelegation = "Vote delegation to always-abstain"
  display AlwaysNoDelegation = "Vote delegation to always-no"

-- | Types of certificate. When registering and delegating in the same transaction,
-- the registration certificate MUST be first.
data CertificateAction
  -- | Register a staking credential and pay the 2 ADA deposit.
  = Registration
  -- | Deregister a staking credential and recover the 2 ADA deposit.
  | Deregistration
  -- | Delegate the staking credential to a pool.
  | StakeDelegation PoolID
  -- | Delegate the staking credential to a drep.
  | VoteDelegation VoteDelegation
  deriving (Show,Eq,Ord)

makePrisms ''CertificateAction

instance Display CertificateAction where
  display Registration = "Stake Registration"
  display Deregistration = "Stake Deregistration"
  display (StakeDelegation (PoolID poolId)) = "Stake delegation to " <> toText poolId
  display (VoteDelegation voteDelegation) = display voteDelegation

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
-- | Information for a staking script.
data StakingScriptInfo = StakingScriptInfo
  -- | The staking script witness.
  { scriptWitness :: ScriptWitness
  -- | The redeemer to use.
  , redeemer :: Redeemer
  -- | The estimated execution budget for this spending execution.
  , executionBudget :: ExecutionBudget
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''StakingScriptInfo

instance ToBuildCmdField StakingScriptInfo where
  toBuildCmdField tmpDir StakingScriptInfo{..} = case scriptWitness of
    ReferenceWitness (ref, _) -> unwords $ filter (/= "")
      [ "--withdrawal-tx-in-reference " <> display ref
      , "--withdrawal-plutus-script-v2"
      , unwords 
          [ "--withdrawal-reference-tx-in-redeemer-file"
          , toText $ redeemerFilePath tmpDir $ hashRedeemer redeemer
          ]
      , "--withdrawal-reference-tx-in-execution-units " <> display executionBudget
      ]
    NormalWitness script -> unwords $ filter (/= "")
      [ "--withdrawal-script-file " <> toText (plutusFilePath tmpDir $ hashScript script)
      , "--withdrawal-redeemer-file " <> toText (redeemerFilePath tmpDir $ hashRedeemer redeemer)
      , "--withdrawal-execution-units " <> display executionBudget
      ]

-- | Information for a particular withdrawal.
data TxBodyWithdrawal = TxBodyWithdrawal
  -- | The staking credential.
  { stakeCredential :: Credential
  -- | The target address.
  , stakeAddress :: StakeAddress
  -- | The amount of lovelace withdrawn from the rewards address.
  , lovelace :: Lovelace
  -- | The information for the staking script execution.
  , stakingScriptInfo :: Maybe StakingScriptInfo
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBodyWithdrawal

instance Ord TxBodyWithdrawal where
  TxBodyWithdrawal{stakeCredential} <= TxBodyWithdrawal{stakeCredential=otherCred} = 
    stakeCredential <= otherCred

instance ToBuildCmdField TxBodyWithdrawal where
  toBuildCmdField tmpDir TxBodyWithdrawal{..} = unwords $ filter (/= "")
    [ "--withdrawal " <> toText stakeAddress <> "+" <> toText lovelace
    , maybe "" (toBuildCmdField tmpDir) stakingScriptInfo
    ]

instance ExportContractFiles TxBodyWithdrawal where
  exportContractFiles TxBodyWithdrawal{stakingScriptInfo} =
    whenJust stakingScriptInfo $ \StakingScriptInfo{..} -> do
      -- | Export the spending script.
      exportScriptWitness scriptWitness

      -- | Export the redeemer file.
      exportRedeemer redeemer

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

-- All mints/burns share a single "--mint" line so this instance must operate on the entire list
-- of mints/burns.
instance ToBuildCmdField [TxBodyMint] where
  toBuildCmdField _ [] = "" -- Don't include if there are no mints.
  toBuildCmdField tmpDir ms = unwords $ filter (/= "")
      [ "--mint"
      -- The assets must be surrounded by quotes.
      , show $ unwords $ intersperse "+" $ for allAssetsMinted $ 
          \NativeAsset{..} -> show quantity <> " " <> display policyId <> "." <> display tokenName
      , unwords $ map mintField ms
      ]
    where
      allAssetsMinted :: [NativeAsset]
      allAssetsMinted = concatMap (view #nativeAssets) ms

      mintField :: TxBodyMint -> Text
      mintField TxBodyMint{..} = case scriptWitness of
        NormalWitness script -> unwords
          [ "--mint-script-file " <> toText (plutusFilePath tmpDir $ hashScript script)
          , "--mint-redeemer-file " <> toText (redeemerFilePath tmpDir $ hashRedeemer redeemer)
          , "--mint-execution-units " <> display executionBudget
          ]
        ReferenceWitness (utxoRef, _) -> unwords
          [ "--mint-tx-in-reference " <> display utxoRef
          , "--mint-plutus-script-v2"
          , "--policy-id " <> show mintingPolicyHash
          , unwords
              [ "--mint-reference-tx-in-redeemer-file"
              , toText (redeemerFilePath tmpDir $ hashRedeemer redeemer)
              ]
          , "--mint-reference-tx-in-execution-units " <> display executionBudget
          ]

mergeTxBodyMints :: [TxBodyMint] -> [TxBodyMint]
mergeTxBodyMints = map removeZeroQuantities
                 . toList
                 . Map.fromListWith sumMints
                 . map (\mint@TxBodyMint{mintingPolicyHash} -> (mintingPolicyHash, mint))
  where
    -- Remove zero native asset quantities, but deliberately leave mints with empty an 
    -- native asset field.
    removeZeroQuantities :: TxBodyMint -> TxBodyMint
    removeZeroQuantities tm = tm & #nativeAssets %~ filter ((/=0) . view #quantity)

    sumMints :: TxBodyMint -> TxBodyMint -> TxBodyMint
    sumMints main@TxBodyMint{nativeAssets} TxBodyMint{nativeAssets=otherAssets} =
      main & #nativeAssets .~ sumNativeAssets (nativeAssets <> otherAssets)

removeEmptyMints :: [TxBodyMint] -> [TxBodyMint]
removeEmptyMints = mapMaybe checkMint
  where
    -- Return nothing if the `TxBodyMint` should be removed from the `TxBody`. Also remove zero
    -- native asset quantities.
    checkMint :: TxBodyMint -> Maybe TxBodyMint
    checkMint tm@TxBodyMint{nativeAssets}
      | null filteredAssets = Nothing
      | otherwise = Just $ tm & #nativeAssets .~ filteredAssets
      where 
        filteredAssets = filter ((/=0) . view #quantity) nativeAssets

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
  -- | The required amount of collateral based on the transaction fee.
  , requiredCollateral :: Lovelace
  } deriving (Show,Eq)

instance ToBuildCmdField TxBodyCollateral where
  toBuildCmdField _ TxBodyCollateral{..} =
    unwords
      [ "--tx-in-collateral " <> display utxoRef
      , "--tx-total-collateral " <> show (unLovelace requiredCollateral)
      , unwords 
          [ "--tx-out-return-collateral"
          -- The collateral change output amount surrounded by quotes.
          , show $ unwords
              [ toText paymentAddress
              , show (unLovelace collateralChange) <> " lovelace"
              ] 
          ]
      ]
    where
      -- This is required due to the dummy fee being 5 ADA. It is easy for the difference to be
      -- negative.
      collateralChange
        | lovelace - requiredCollateral < 0 = 0
        | otherwise = lovelace - requiredCollateral

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
  -- | The keys required for smart contract executions.
  , requiredWitnesses :: [KeyWitness]
  -- | The keys that must sign the transaction. This includes the keys in `requiredWitnesses`.
  , keyWitnesses :: [KeyWitness]
  , fee :: Lovelace
  -- | The invalid before slot number.
  , invalidBefore :: Maybe Slot
  -- | The invalid hereafter slot number.
  , invalidHereafter :: Maybe Slot
  -- | The network this transaction is for.
  , network :: Network
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
        -- Sort the withdrawals by address. This makes getting the execution budgets easier.
        sortOn (view #stakeAddress) $ txBody1 ^. #withdrawals <> txBody2 ^. #withdrawals
    , mints = 
        -- Sort them by policy id. This makes getting the execution budgets easier.
        -- Redeemer and witness collisions must be caught before converting to `TxBody`!
        sort $ txBody1 ^. #mints <> txBody2 ^. #mints
    , collateralInput = 
       -- There should only be one txBody with collateral set.
        txBody1 ^. #collateralInput <|> txBody2 ^. #collateralInput
    , requiredWitnesses = 
        -- Remove duplicates.
        ordNub $ txBody1 ^. #requiredWitnesses <> txBody2 ^. #requiredWitnesses
    , keyWitnesses = 
        -- Remove duplicates.
        ordNub $ txBody1 ^. #keyWitnesses <> txBody2 ^. #keyWitnesses
    , fee = 
        -- Most txBodies will have a fee of zero so the non-zero one must be chosen.
        -- I do not think there is a case where two txBodies will both have fees set.
        max (txBody1 ^. #fee) (txBody2 ^. #fee)
    , invalidBefore = 
        -- When two txBodies use invalidBefore, the more restrictive one should be used.
        updateInvalidBefore (txBody1 ^. #invalidBefore) (txBody2 ^. #invalidBefore)
    , invalidHereafter = 
        -- When two txBodies use invalidHereafter, the more restrictive one should be used.
        updateInvalidHereafter (txBody1 ^. #invalidHereafter) (txBody2 ^. #invalidHereafter)
    , network = 
        if txBody1 ^. #network /= txBody2 ^. #network 
        then error "Networks don't match" -- This should never happen.
        else txBody1 ^. #network
    }

instance Monoid TxBody where
  mempty = TxBody
    { inputs = []
    , outputs = []
    , certificates = []
    , withdrawals = []
    , mints = []
    , collateralInput = Nothing
    , requiredWitnesses = []
    , keyWitnesses = []
    , fee = 0
    , invalidBefore = Nothing
    , invalidHereafter = Nothing
    , network = def
    }

instance ExportContractFiles TxBody where
  exportContractFiles TxBody{..} = do
    -- Export all files for the necessary minting policies.
    mapM_ exportContractFiles mints

    -- Export all files for the necessary spending scripts.
    mapM_ exportContractFiles inputs

    -- Export all files for the necessary staking scritps.
    mapM_ exportContractFiles withdrawals

    -- Export all files for the necessary outputs.
    mapM_ exportContractFiles outputs

-------------------------------------------------
-- `AddToTxBody` Class
-------------------------------------------------
class AddToTxBody a where
  -- Some highlevel actions impact multiple parts of the transaction.
  addToTxBody :: TxBody -> a -> TxBody

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Calculate the total size of all reference scripts being used in the transaction. This
-- number is needed to calculate the transaction fee.
calcTotalReferenceScriptSize :: TxBody -> Integer
calcTotalReferenceScriptSize TxBody{..} = Map.foldl' (+) 0 referenceMap
  where
    -- Build up a map from TxOutRef to size so that references are only counted once.
    referenceMap :: Map.Map TxOutRef Integer
    referenceMap = Map.fromList $ mconcat
      [ mapMaybe (preview $ #spendingScriptInfo % _Just % #scriptWitness % _ReferenceWitness) inputs
      , mapMaybe (preview $ #stakingScriptInfo % _Just % #scriptWitness % _ReferenceWitness) withdrawals
      , mapMaybe (preview $ #scriptWitness % _ReferenceWitness) mints
      ]

updateInvalidBefore :: Maybe Slot -> Maybe Slot -> Maybe Slot
updateInvalidBefore = updateBound max

updateInvalidHereafter :: Maybe Slot -> Maybe Slot -> Maybe Slot
updateInvalidHereafter = updateBound min

updateBound :: (Slot -> Slot -> Slot) -> Maybe Slot -> Maybe Slot -> Maybe Slot
updateBound selector newBound oldBound = case (newBound, oldBound) of
  (Just bound1, Just bound2) -> Just $ selector bound1 bound2
  (x,y) -> x <|> y
