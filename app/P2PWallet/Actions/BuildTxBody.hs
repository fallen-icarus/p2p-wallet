module P2PWallet.Actions.BuildTxBody
  (
    buildTxBody
  ) where

import System.FilePath ((</>), (<.>))
import Data.Aeson (encode,parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.String qualified as String

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Koios.BudgetEstimations
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | The builder steps. This is useful for creating the builder.log file which shows the exact
-- commands that were used to create the final transaction.
data BuilderStep
  -- | Create a fresh builder.log file.
  = InitializeLogFile
  -- | Add the command that is about to be executed to the builder log.
  | BuildCommandStep String
  -- | Add the exection budgets to the builder log.
  | ExecutionBudgetStep [BudgetEstimation]

-- | Log the build step.
logBuilderStep :: BuilderLogFile -> BuilderStep -> IO ()
logBuilderStep (BuilderLogFile file) step = case step of
  InitializeLogFile -> 
    -- Clear the log file from last time.
    writeFile file "# Build Logs:\n"
  BuildCommandStep cmd -> 
    -- Add the command that is about to be executed.
    appendFile file ("\n\n" <> cmd)
  ExecutionBudgetStep budgets -> 
    -- The execution budgets are not a command but are needed for later steps. They are shown as
    -- commented json objects in the builder.log file.
    appendFile file $ ("\n\n# Estimated budgets using previous tx.body:\n" <>) $ 
      String.unlines $ map (("# " <>) . show . encode) budgets

-- | Log the command before actually running it.
runBuildCmd_ :: BuilderLogFile -> String -> IO ()
runBuildCmd_ builderLogFile cmd = 
  logBuilderStep builderLogFile (BuildCommandStep cmd) >> runCmd_ cmd

-- | Log the command before actually running it. Return the results of the command.
runBuildCmd :: BuilderLogFile -> String -> IO String
runBuildCmd builderLogFile cmd = 
  logBuilderStep builderLogFile (BuildCommandStep cmd) >> runCmd cmd

-- | Since `cardano-cli transaction build-raw` can throw confusing error messages if certain pieces
-- are missing from the transaction, this check validates those cases will not occur. It will throw
-- an `AppError` with a more user friendly error message if cardano-cli is likely to throw an error.
-- The UI should catch these errors sooner but the redundant checks can't hurt and enable building
-- to be tested without a UI.
canBeBuilt :: TxBuilderModel -> IO ()
canBeBuilt TxBuilderModel{userInputs,changeOutput,collateralInput,requiresCollateral}
  | null userInputs = throwIO $ AppError "No inputs specified."
  -- A change address must be specified.
  | change ^. #paymentAddress == "" = throwIO $ AppError "Change address missing."
  -- The value of ADA must be balanced.
  | change ^. #lovelace < 0 = throwIO $ AppError "Ada is not balanced."
  -- The value of the native assets must be balanced.
  | nativeAssetsNotBalanced = throwIO $ AppError "Native assets are not balanced."
  -- If it requires collateral, then a collateral input must be set.
  | requiresCollateral && isNothing collateralInput =
      throwIO $ AppError "Transaction requires collateral."
  | otherwise = return ()
  where
    change :: ChangeOutput
    change = fromMaybe def changeOutput

    nativeAssetsNotBalanced :: Bool
    nativeAssetsNotBalanced = any ((<0) . view #quantity) $ change ^. #nativeAssets

-- | Build the transaction and generate the tx.body file.
buildTxBody :: Network -> TxBuilderModel -> IO TxBuilderModel
buildTxBody network tx = do
  -- Check if the transaction can be built without errors. 
  canBeBuilt tx

  -- Get the file names since cardano-cli works with files.
  tmpDir <- TmpDirectory <$> getTemporaryDirectory
  let paramsFile = ParamsFile $ toString tmpDir </> "params" <.> "json"
      txBodyFile = TxBodyFile $ toString tmpDir </> "tx" <.> "body"
      builderLogFile = BuilderLogFile $ toString tmpDir </> "builder" <.> "log"

  -- Create a fresh log file for this build iteration.
  logBuilderStep builderLogFile InitializeLogFile

  -- Get the current parameters if necessary, and write them to a file for cardano-cli to use.
  -- If wallets have already been synced since starting the app, the parameters should already be
  -- saved in the `TxBuilderModel`.
  parameters <- maybe (runGetParams network >>= fromRightOrAppError) return $ tx ^. #parameters
  writeFileBS (toString paramsFile) parameters

  -- Convert the model to `TxBody` and extract out the required witnesses for returning
  -- with the finalized `TxBuilderModel`.
  let initialTxBody@TxBody{keyWitnesses,certificates} = convertToTxBody $ 
        -- Initialize the fee to 5 ADA so previous builds don't influence this interation's fee
        -- calculation. It is set to 5 ADA because the execution budget calculation is slightly
        -- impacted by the fee amount. 5 ADA should be an over-estimate in every case and when
        -- the actual fee is calculated, setting it to the new fee should not impact the execution
        -- budgets.
        tx & #fee .~ 5_000_000

  -- Build the certificate files so they are available in the tmp directory. Registrations must
  -- appear first in the transaction. 
  certificateFiles <- mapM (buildCertificate tmpDir builderLogFile) certificates

  -- Calculate the budgets if necessary.
  budgets <- if not (tx ^. #requiresCollateral) then return Nothing else do
      -- Any plutus scripts that are used locally must be exported.  The redeemers and datums used
      -- must also be exported. All files will be located in the tmp directory and will have their
      -- hashes as the file names.
      exportContractFiles initialTxBody

      -- Create the intitial tx.body file.
      runBuildCmd_ builderLogFile $ 
        buildRawCmd tmpDir paramsFile txBodyFile certificateFiles initialTxBody

      -- Use the tx.body file to calculate the execution units.
      Just <$> estimateExecutionBudgets network builderLogFile txBodyFile

  -- Calculate the fee. This is unfortunately a moving target since updating the fee inevitably
  -- changes the required fee. To account for this, the calculation is done twice (updating
  -- the fee before the second calculation).
  fee <- do
      feeCalc1 <- 
        estimateTxFee network tmpDir builderLogFile paramsFile txBodyFile certificateFiles $ 
          -- The original `TxBody` does not have the updated budgets so it must be updated
          -- before calculating the fee.
          updateBudgets budgets initialTxBody
      
      -- Calculate the fee a second time.
      estimateTxFee network tmpDir builderLogFile paramsFile txBodyFile certificateFiles $
        -- The result of `convertToTxBody` does not have the calculated budgets so they must be
        -- added again.
        updateBudgets budgets $ 
          -- Replace the fee and rebalance.
          convertToTxBody $ balanceTx $ tx & #fee .~ feeCalc1

  -- Update the `TxBuilderModel` with the results.
  let finalizedTx = 
        balanceTx $ tx 
          -- Set the fee.
          & #fee .~ fee
          -- Tell the app which key witnesses are required.
          & #keyWitnesses .~ keyWitnesses
          -- Determine whether the app knows all of the required keys.
          & #allKeyWitnessesKnown .~ all (isJust . snd . unKeyWitness) keyWitnesses
          -- If the parameters were synced during this build, they should be saved for next
          -- time.
          & #parameters ?~ parameters

  -- Build the transaction one more time so that the tx.body file has the finalized transaction.
  -- Also set the witnesses since the app will need them to determine what actions can be taken
  -- on the tx.body file.
  runBuildCmd_ builderLogFile $ buildRawCmd tmpDir paramsFile txBodyFile certificateFiles $ 
    -- The budgets need to be added again.
    updateBudgets budgets $ convertToTxBody finalizedTx

  -- Return the updated `TxBuilderModel`. Mark the model as built; this must be done last in case
  -- there is an error with any of the previous steps. If the tx was marked as built too soon, it
  -- could cause confusing behavior.
  return $ finalizedTx & #isBuilt .~ True

-- | Build all required certificates and return the filepaths used. Since the transaction
-- can contain multiple certificates for a given stake address, the file name is suffixed with
-- the action to keep the certificate files distinct. Later certificate actions override previous
-- actions! This function will log the command used to create the certificate.
buildCertificate :: TmpDirectory -> BuilderLogFile -> TxBodyCertificate -> IO CertificateFile
buildCertificate tmpDir builderLogFile TxBodyCertificate{stakeAddress,certificateAction} = do
  let certFile suffix = toString tmpDir </> (toString stakeAddress <> "_" <> suffix) <.> "cert"
  case certificateAction of
    Registration -> do
      let -- Suffix the file name with the action.
          fullFileName = certFile "registration"

      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ toString $ unwords
        [ "cardano-cli stake-address registration-certificate"
        , "--stake-address " <> toText stakeAddress
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName
    Deregistration -> do
      let -- Suffix the file name with the action.
          fullFileName = certFile "deregistration"
          

      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ toString $ unwords
        [ "cardano-cli stake-address deregistration-certificate"
        , "--stake-address " <> toText stakeAddress
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName
    Delegation (PoolID poolID) -> do
      let -- Suffix the file name with the action.
          fullFileName = certFile "delegation"
      
      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ toString $ unwords
        [ "cardano-cli stake-address delegation-certificate"
        , "--stake-address " <> toText stakeAddress
        , "--stake-pool-id " <> poolID
        , "--out-file " <> toText (certFile "delegation")
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName

-- | Calculate the fee.
estimateTxFee 
  :: Network 
  -> TmpDirectory 
  -> BuilderLogFile
  -> ParamsFile 
  -> TxBodyFile 
  -> [CertificateFile] 
  -> TxBody 
  -> IO Lovelace
estimateTxFee network tmpDir builderLogFile paramsFile txBodyFile certificateFiles tx@TxBody{..} = do
    -- This builds the tx.body file and stores it in the tmp directory. 
    runBuildCmd_ builderLogFile $ buildRawCmd tmpDir paramsFile txBodyFile certificateFiles tx

    -- This uses the tx.body file to estimate the transaction fee. 
    fromJustOrAppError "Could not parse min fee." . fmap Lovelace . readMaybe =<<
      runBuildCmd builderLogFile calcFeeCmd
  where
    numberOfInputs :: Int
    numberOfInputs = sum
      [ length inputs
      , maybe 0 (const 1) collateralInput
      ]

    numberOfOutputs :: Int
    numberOfOutputs = sum
      [ length outputs
      -- The collateral change output will be present if there is a collateral input.
      , maybe 0 (const 1) collateralInput 
      ]

    -- This is formatted to be human-readable since it will be added to the builder.log file.
    calcFeeCmd :: String
    calcFeeCmd =
      toString $ unlines
        [ "(cardano-cli transaction calculate-min-fee \\"
        , "--tx-body-file " <> toText txBodyFile <> " \\"
        , toNetworkFlag network <> " \\"
        , "--protocol-params-file " <> toText paramsFile <> " \\"
        , "--tx-in-count " <> show numberOfInputs <> " \\"
        , "--tx-out-count " <> show numberOfOutputs <> " \\"
        , unwords 
            [ "--witness-count " <> show (length keyWitnesses) <> ")"
            , "| cut -d' ' -f1"
            ]
        ]

-- | Create the actual `cardano-cli transaction build-raw` command. This is formatted to be 
-- human-readable since it will be added to the builder.log file.
buildRawCmd 
  :: TmpDirectory 
  -> ParamsFile 
  -> TxBodyFile 
  -> [CertificateFile] 
  -> TxBody 
  -> String
buildRawCmd tmpDir paramsFile outFile certificateFiles TxBody{..} = 
    toString $ unlines $ 
      -- Filter out unused fields.
      filter (/= "")
        [ "cardano-cli transaction build-raw \\"
        , innerUnlines $ map inputField inputs
        , innerUnlines $ map outputField outputs
        , totalMint $ concatMap (view #nativeAssets) mints
        , innerUnlines $ map mintField mints
        , innerUnlines $ map withdrawalField withdrawals
        , innerUnlines $ for certificateFiles $ \certFile -> 
            "--certificate-file " <> toText certFile <> "  \\"
        , maybe "" collateralField collateralInput
        , "--protocol-params-file " <> toText paramsFile <> " \\"
        , "--fee " <> show (unLovelace fee) <> " \\"
        , "--out-file " <> toText outFile
        ]
  where
    -- `unlines` adds a newline at the end which would break up the stanza. This
    -- version does not add the final newline. 
    innerUnlines :: [Text] -> Text
    innerUnlines = mconcat . intersperse "\n"

    plutusFile :: ScriptHash -> String
    plutusFile scriptHash = toString tmpDir </> show scriptHash <.> "plutus"

    redeemerFile :: RedeemerHash -> String
    redeemerFile redeemerHash = toString tmpDir </> show redeemerHash <.> "json"

    -- datumFile :: DatumHahs -> Text
    -- datumFile datumHash = toText tmpDir </> show datumHash <.> "json"

    inputField :: TxBodyInput -> Text
    inputField TxBodyInput{utxoRef} = innerUnlines
      [ "--tx-in " <> display utxoRef <> " \\"
      ]

    outputField :: TxBodyOutput -> Text
    outputField TxBodyOutput{..} = innerUnlines
      [ unwords
          [ "--tx-out"
          -- The output amount surrounded by quotes.
          , show $ unwords
              [ toText paymentAddress
              , show (unLovelace lovelace) <> " lovelace"
              , unwords $ for nativeAssets $ \NativeAsset{..} ->
                  "+ " <> show quantity <> " " <> display policyId <> "." <> display tokenName
              ] 
          , "\\"
          ]
      ]

    withdrawalField :: TxBodyWithdrawal -> Text
    withdrawalField TxBodyWithdrawal{..} = innerUnlines
      [ "--withdrawal" <> toText stakeAddress <> "+" <> toText lovelace <> " \\"
      ]

    collateralField :: TxBodyCollateral -> Text
    collateralField TxBodyCollateral{utxoRef,lovelace,paymentAddress} = 
      -- The hard-coded collateral amount. I have never seen a transaction require 4 ADA
      -- as collateral.
      let actualCollateral = Lovelace 4_000_000 in
      innerUnlines
        [ "--tx-in-collateral " <> display utxoRef <> " \\"
        , "--tx-total-collateral " <> show (unLovelace actualCollateral) <> " \\"
        , unwords 
            [ "--tx-out-return-collateral"
            -- The collateral change output amount surrounded by quotes.
            , show $ unwords
                [ toText paymentAddress
                , show (unLovelace $ lovelace - actualCollateral) <> " lovelace"
                ] 
            , "\\"
            ]
        ]

    -- The transaction only uses a single `--mint` field no matter how many different policys are
    -- executed.
    totalMint :: [NativeAsset] -> Text
    totalMint [] = "" -- Don't include if there are no mints.
    totalMint allAssetsMinted = unwords
      [ "--mint"
      -- The assets must be surrounded by quotes.
      , show $ unwords $ intersperse "+" $ for allAssetsMinted $ 
          \NativeAsset{..} -> show quantity <> " " <> display policyId <> "." <> display tokenName
      , "\\"
      ]

    mintField :: TxBodyMint -> Text
    mintField TxBodyMint{..} = case scriptWitness of
      NormalWitness script -> 
        innerUnlines
          [ "--mint-script-file " <> toText (plutusFile $ hashScript script) <> " \\"
          , "--mint-redeemer-file " <> toText (redeemerFile $ hashRedeemer redeemer) <> " \\"
          , "--mint-execution-units " <> display executionBudget <> " \\"
          ]
      ReferenceWitness utxoRef -> 
        innerUnlines
          [ "--mint-tx-in-reference " <> display utxoRef <> " \\"
          , "--mint-plutus-script-v2" <> " \\"
          , "--mint-reference-tx-in-redeemer-file " <> toText (redeemerFile $ hashRedeemer redeemer) <> " \\"
          , "--mint-reference-tx-in-execution-units " <> display executionBudget <> " \\"
          ]

-- | Estimate the execution budget for each smart contract in the transaction. Log the results in
-- the builder.log file before returning them.
estimateExecutionBudgets :: Network -> BuilderLogFile -> TxBodyFile -> IO [BudgetEstimation]
estimateExecutionBudgets network builderLogFile (TxBodyFile txBodyFile) = do
  runEvaluateTx network txBodyFile >>= \case
    Left err -> throwIO $ AppError err
    Right r -> do 
      -- It was returned as `Value` so the result must still be decoded.
      let mBudgets = unEstimationResult <$> parseMaybe parseJSON r
          mErrMsg = unEstimationError <$> parseMaybe parseJSON r

      -- Terminate this function and throw the error if estimation failed.
      whenJust mErrMsg $ \msg -> throwIO $ AppError $ showValue msg

      case mBudgets of
        Nothing -> throwIO $ AppError $ "Could not parse response:\n\n" <> showValue r
        Just budgets -> do
          -- Log the budgets.
          logBuilderStep builderLogFile $ ExecutionBudgetStep budgets
          -- Return the budgets.
          return budgets

-- | Update the budgets for the `TxBody`. The result of `convertToTxBody` does not have the
-- budgets set so they will need to be added after each conversion. This is called even when
-- budgets aren't necessary so the `Maybe` is used to dictate whether any changes are actually
-- needed.
updateBudgets :: Maybe [BudgetEstimation] -> TxBody -> TxBody
updateBudgets Nothing tx = tx
updateBudgets (Just budgets) tx = foldl' updateBudget tx budgets
  where
    -- The validators are assumed to be in the same order as was evaluated.
    updateBudget :: TxBody -> BudgetEstimation -> TxBody
    updateBudget oldTx BudgetEstimation{..} = case validatorIndex of
      Spending _ -> oldTx
      Minting idx -> 
        -- Replace the old execution budgets with the newly calculated ones.
        oldTx & #mints % ix idx % #executionBudget .~ executionBudget
