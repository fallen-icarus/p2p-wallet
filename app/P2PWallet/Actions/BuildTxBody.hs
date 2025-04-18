module P2PWallet.Actions.BuildTxBody
  (
    buildTxBody
  ) where

import System.FilePath ((</>), (<.>))
import Data.Aeson (encode,parseJSON)
import Data.Aeson.Types (parseMaybe)

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Koios.BudgetEstimations
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
    writeFile file "# Build Logs:"
  BuildCommandStep cmd -> 
    -- Add the command that is about to be executed.
    appendFile file ("\n\n" <> cmd)
  ExecutionBudgetStep budgets -> 
    -- The execution budgets are not a command but are needed for later steps. They are shown as
    -- commented json objects in the builder.log file.
    appendFile file $ mconcat
      [ "\n\n# Estimated budgets using previous tx.body:\n"
      , mconcat $ intersperse "\n" $ map (("# " <>) . show . encode) budgets
      , "\n#\n" -- extra space
      , ("# Total CPU: " <>) $ show $ foldl' (\acc i -> acc + i ^. #executionBudget % #cpu) 0 budgets
      , ("\n# Total Memory: " <>) $ show $ foldl' (\acc i -> acc + i ^. #executionBudget % #memory) 0 budgets
      ]

-- | Log the command before actually running it. The command will be formatted to make it more
-- human-readable.
runBuildCmd_ :: BuilderLogFile -> Text -> IO ()
runBuildCmd_ builderLogFile cmd = 
  let formattedCmd = toString $ prettyFormatCmd cmd in
  logBuilderStep builderLogFile (BuildCommandStep formattedCmd) >> runCmd_ formattedCmd

-- | Log the command before actually running it. Return the results of the command. The command will
-- be formatted to make it more human-readable.
runBuildCmd :: BuilderLogFile -> Text -> IO String
runBuildCmd builderLogFile cmd = 
  let formattedCmd = toString $ prettyFormatCmd cmd in
  logBuilderStep builderLogFile (BuildCommandStep formattedCmd) >> runCmd formattedCmd

-- | Check whether the change output has enough ada. This should always be called _after_ getting
-- the parameters so there is no need to query for them again.
changeAdaValueCheck :: Network -> ByteString -> ChangeOutput -> IO ()
changeAdaValueCheck network parameters changeOutput@ChangeOutput{lovelace} = do
  -- This must be checked first since a negative value will cause `calculateMinUTxOValue` to throw
  -- an unfriendly error.
  when (lovelace < 0) $
    throwIO $ AppError $ unlines
      [ unwords 
          [ "There is not enough ADA change to cover the fee. The estimated extra ADA required"
          , "is " <> display (abs lovelace) <> "."
          ]
      , ""
      , "Add another input to increase the amount of ADA as change."
      , "NOTE: This will require a re-calculation of the fee."
      ]

  minUTxOValue <-
    calculateMinUTxOValue network (Just parameters) changeOutput >>= 
      fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead

  when (minUTxOValue > lovelace) $ 
    throwIO $ AppError $ changeHasTooLittleAdaMsg minUTxOValue
  
-- | Check whether there is enough collateral for the transaction.
collateralValueCheck :: TxBuilderModel -> IO ()
collateralValueCheck tx = 
  whenJust (convertToTxBody tx ^. #collateralInput) $ 
    \TxBodyCollateral{lovelace,requiredCollateral} -> 
      when (lovelace - requiredCollateral < 0) $
        throwIO $ AppError $ unlines
          [ "There is not enough collateral for this transaction."
          , unwords
              [ display requiredCollateral
              , "is required, but only"
              , display lovelace
              , "is available."
              ]
          , "Choose a different collateral input with at least " <> display requiredCollateral <> "."
          ]

-- | Build the transaction and generate the tx.body file.
buildTxBody :: Network -> TxBuilderModel -> IO TxBuilderModel
buildTxBody network tx = do
  -- Check if the transaction can be built without errors. 
  whenLeft_ (canBeBuilt tx) (throwIO . AppError)

  -- Get the file names since cardano-cli works with files.
  tmpDir <- TmpDirectory <$> getTemporaryDirectory
  let paramsFile = ParamsFile $ toString tmpDir </> "params" <.> "json"
      txBodyFile = TxBodyFile $ toString tmpDir </> "tx" <.> "body"
      builderLogFile = BuilderLogFile $ toString tmpDir </> "builder" <.> "log"
      -- This will over-write the old tx.body file.
      transformedTxFile = TransformedTxFile $ toString tmpDir </> "tx" <.> "body"

  -- Create a fresh log file for this build iteration.
  logBuilderStep builderLogFile InitializeLogFile

  -- Get the current parameters if necessary, and write them to a file for cardano-cli to use.
  -- If wallets have already been synced since starting the app, the parameters should already be
  -- saved in the `TxBuilderModel`.
  parameters@(allParameters, collateralPercentage) <- 
    maybe (runGetParams network >>= fromRightOrAppError) return $ tx ^. #parameters
  writeFileBS (toString paramsFile) allParameters

  -- Create the template tx that all intermediate transactions will be derived from. The original
  -- tx cannot be used because the collateral input does not have the collateralPercentage set yet.
  let templateTx = tx & #collateralInput % _Just % #collateralPercentage .~ collateralPercentage

  -- Convert the model to `TxBody` and extract out the required witnesses for returning
  -- with the finalized `TxBuilderModel`.
  let initialTxBody@TxBody{keyWitnesses,certificates} = convertToTxBody $ 
        -- Initialize the fee to 5 ADA so previous builds don't influence this interation's fee
        -- calculation. It is set to 5 ADA because the execution budget calculation is slightly
        -- impacted by the fee amount. 5 ADA should be an over-estimate in every case and when
        -- the actual fee is calculated, setting it to the new fee should not impact the execution
        -- budgets.
        templateTx & #fee .~ 5_000_000

  -- Build the certificate files so they are available in the tmp directory. Registrations must
  -- appear first in the transaction. 
  certificateFiles <- mapM (buildCertificate tmpDir builderLogFile) certificates

  -- Calculate the budgets if necessary.
  budgets <- if not (templateTx ^. #requiresCollateral) then return Nothing else do
      -- Any plutus scripts that are used locally must be exported. The redeemers and datums used
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
        estimateTxFee tmpDir builderLogFile paramsFile txBodyFile certificateFiles $ 
          -- The original `TxBody` does not have the updated budgets so it must be updated
          -- before calculating the fee.
          updateBudgets budgets initialTxBody
      
      -- Calculate the fee a second time after adding the first calculated fee.
      let updatedTx@TxBuilderModel{changeOutput} = balanceTx $ templateTx & #fee .~ feeCalc1

      -- `balanceTx` will subtract the fee from the change which can result in a negative change.
      -- The `changeOutput` should always be `Just`.
      fromJustOrAppError "changeOutput is Nothing" changeOutput >>=
        changeAdaValueCheck network allParameters

      estimateTxFee tmpDir builderLogFile paramsFile txBodyFile certificateFiles $
        -- The result of `convertToTxBody` does not have the calculated budgets so they must be
        -- added again.
        updateBudgets budgets $ convertToTxBody updatedTx

  -- Update the `TxBuilderModel` with the results.
  let finalizedTx = 
        balanceTx $ templateTx 
          -- Set the fee.
          & #fee .~ fee
          -- Tell the app which key witnesses are required.
          & #keyWitnesses .~ keyWitnesses
          -- If the parameters were synced during this build, they should be saved for next
          -- time.
          & #parameters ?~ parameters

  -- Check that the change output has enough ada now that the final fee has been subtracted
  -- (as part of `balanceTx`.)
  changeAdaValueCheck network allParameters $ fromMaybe def $ finalizedTx ^. #changeOutput

  -- Verify that the transaction has enough collateral.
  collateralValueCheck finalizedTx

  -- Build the transaction one more time so that the tx.body file has the finalized transaction.
  -- Also set the witnesses since the app will need them to determine what actions can be taken
  -- on the tx.body file.
  runBuildCmd_ builderLogFile $ buildRawCmd tmpDir paramsFile txBodyFile certificateFiles $ 
    -- The budgets need to be added again.
    updateBudgets budgets $ convertToTxBody finalizedTx

  trace "After cardano-hw-cli Issue #177 is fixed, this should be enabled for all transactions" $ 
    when (finalizedTx ^. #txType /= WatchedTx) $
      -- Convert the tx.body file to the proper CBOR format for using hardware wallets.
      -- The old tx.body file will be over-written. 
      runBuildCmd_ builderLogFile $ transformTxBodyCmd txBodyFile transformedTxFile

  -- Return the updated `TxBuilderModel`. Mark the model as built; this must be done last in case
  -- there is an error with any of the previous steps. If the tx was marked as built too soon, it
  -- could cause confusing behavior.
  return $ finalizedTx & #isBuilt .~ True

-- | Build all required certificates and return the filepaths used. Since the transaction
-- can contain multiple certificates for a given stake address, the file name is suffixed with
-- the action to keep the certificate files distinct. Later certificate actions override previous
-- actions! This function will log the command used to create the certificate.
buildCertificate :: TmpDirectory -> BuilderLogFile -> TxBodyCertificate -> IO CertificateFile
buildCertificate tmpDir builderLogFile TxBodyCertificate{stakeCredential,stakeAddress,certificateAction} = do
  let certFile suffix = toString tmpDir </> (toString (display stakeCredential) <> "_" <> suffix) <.> "cert"
  case certificateAction of
    Registration -> do
      -- Suffix the file name with the action.
      let fullFileName = certFile "registration"

      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ unwords
        [ "cardano-cli conway stake-address registration-certificate"
        , "--key-reg-deposit-amt 2000000" -- 2 ADA
        , "--stake-address " <> toText stakeAddress
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName
    Deregistration -> do
      -- Suffix the file name with the action.
      let fullFileName = certFile "deregistration"
          

      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ unwords
        [ "cardano-cli conway stake-address deregistration-certificate"
        , "--key-reg-deposit-amt 2000000" -- 2 ADA
        , "--stake-address " <> toText stakeAddress
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName
    StakeDelegation (PoolID poolID) -> do
      -- Suffix the file name with the action.
      let fullFileName = certFile "stake_delegation"
      
      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ unwords
        [ "cardano-cli conway stake-address stake-delegation-certificate"
        , "--stake-address " <> toText stakeAddress
        , "--stake-pool-id " <> poolID
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName
    VoteDelegation deleg -> do
      -- Suffix the file name with the action.
      let fullFileName = certFile "vote_delegation"
          drepFlag = case deleg of
            AlwaysAbstainDelegation -> "--always-abstain"
            AlwaysNoDelegation -> "--always-no-confidence"
            DRepDelegation drepId isScript ->
              if isScript then 
                unwords
                 [ "--drep-script-hash"
                 , fromRight "could not decode bech32 drep id" $ drepIdToHex drepId
                 ]
              else 
                unwords
                  [ "--drep-key-hash"
                  , display drepId -- This removes the CIP-129 prefix if present.
                  ]
      
      -- Create the certificate file.
      runBuildCmd_ builderLogFile $ unwords
        [ "cardano-cli conway stake-address vote-delegation-certificate"
        , "--stake-address " <> toText stakeAddress
        , drepFlag
        , "--out-file " <> toText fullFileName
        ]

      -- Return the suffixed file name.
      return $ CertificateFile fullFileName

-- | Calculate the fee.
estimateTxFee 
  :: TmpDirectory 
  -> BuilderLogFile
  -> ParamsFile 
  -> TxBodyFile 
  -> [CertificateFile] 
  -> TxBody 
  -> IO Lovelace
estimateTxFee tmpDir builderLogFile paramsFile txBodyFile certificateFiles tx@TxBody{..} = do
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

    calcFeeCmd :: Text
    calcFeeCmd = unwords
      [ "(cardano-cli conway transaction calculate-min-fee"
      , "--tx-body-file " <> toText txBodyFile
      , toNetworkFlag network
      , "--protocol-params-file " <> toText paramsFile
      , "--tx-in-count " <> show numberOfInputs
      , "--tx-out-count " <> show numberOfOutputs
      , "--reference-script-size " <> show (calcTotalReferenceScriptSize tx)
      , unwords 
          [ "--witness-count " <> show (length keyWitnesses) <> ")"
          , "| cut -d' ' -f1"
          ]
      ]

-- | Create the actual `cardano-cli transaction build-raw` command. This is formatted to be 
-- human-readable since it will be added to the builder.log file.
buildRawCmd :: TmpDirectory -> ParamsFile -> TxBodyFile -> [CertificateFile] -> TxBody -> Text
buildRawCmd tmpDir paramsFile outFile certificateFiles TxBody{..} = 
  -- Filter out unused fields.
  unwords $ filter (/= "")
    [ "cardano-cli conway transaction build-raw"
    , unwords $ map (toBuildCmdField tmpDir) inputs
    , unwords $ map (toBuildCmdField tmpDir) outputs
    , toBuildCmdField tmpDir mints
    , unwords $ map (toBuildCmdField tmpDir) withdrawals
    , unwords $ for certificateFiles $ 
        \certFile -> "--certificate-file " <> toText certFile
    , unwords $ for requiredWitnesses $ 
        \(KeyWitness (_,kh,_)) -> "--required-signer-hash " <> show kh
    , maybe "" (toBuildCmdField tmpDir) collateralInput
    , maybe "" (\slot -> "--invalid-before " <> display slot) invalidBefore
    , maybe "" (\slot -> "--invalid-hereafter " <> display slot) invalidHereafter
    , "--protocol-params-file " <> toText paramsFile
    , "--fee " <> show (unLovelace fee)
    , "--out-file " <> toText outFile
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
      Spending idx ->
        -- Replace the old execution budgets with the newly calculated ones.
        oldTx & #inputs % ix idx % #spendingScriptInfo % _Just % #executionBudget .~ executionBudget
      Minting idx -> 
        -- Replace the old execution budgets with the newly calculated ones.
        oldTx & #mints % ix idx % #executionBudget .~ executionBudget
      Withdrawing idx ->
        -- Replace the old execution budgets with the newly calculated ones.
        oldTx & #withdrawals % ix idx % #stakingScriptInfo % _Just % #executionBudget .~ executionBudget

-- | The command to convert the transaction to the proper cbor format.
transformTxBodyCmd :: TxBodyFile -> TransformedTxFile -> Text
transformTxBodyCmd txBodyFile transformedTxFile = 
    fromString $ printf cmdTemplate (toString txBodyFile) (toString transformedTxFile)
  where
    cmdTemplate = "cardano-hw-cli transaction transform --tx-file %s --out-file %s" 
