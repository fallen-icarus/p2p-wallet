module P2PWallet.Actions.BuildTxBody
  (
    buildTxBody
  ) where

import System.FilePath ((</>), (<.>))

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-- Since `cardano-cli transaction build-raw` can throw confusing error messages if certain pieces
-- are missing from the transaction, this check validates those cases will not occur. It will throw
-- an `AppError` with a more user friendly error message if cardano-cli is likely to throw an error.
-- The UI should catch these errors sooner but the redundant checks can't hurt and enable building
-- to be tested without a UI.
canBeBuilt :: TxBuilderModel -> IO ()
canBeBuilt TxBuilderModel{userInputs,changeOutput}
  -- The tx must have inputs.
  | null userInputs = throwIO $ AppError "No inputs specified."
  -- A change address must be specified.
  | change ^. #paymentAddress == "" = throwIO $ AppError "Change address missing."
  -- The value of ADA must be balanced.
  | change ^. #lovelace < 0 = throwIO $ AppError "Ada is not balanced."
  -- The value of the native assets must be balanced.
  | nativeAssetsNotBalanced = throwIO $ AppError "Native assets are not balanced."
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
  tmpDir <- getTemporaryDirectory
  let paramsFile = ParamsFile $ tmpDir </> "params" <.> "json"
      txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"

  -- Get the current parameters, and write them to a file for cardano-cli to use.
  runGetParams network >>= fromRightOrAppError >>= writeFileBS (toString paramsFile)

  -- Convert the model to `TxBody` and extract out the required witnesses for returning
  -- with the finalized `TxBuilderModel`.
  let initialTxBody@TxBody{witnesses} = convertToTxBody tx

  -- Calculate the fee. This calculation is done twice to account for adding the fee the second
  -- time.
  fee <- do
    feeCalc1 <- estimateTxFee network paramsFile txBodyFile initialTxBody
    estimateTxFee network paramsFile txBodyFile $
      -- Add the fee to the transaction, rebalance, and calculate again.
      convertToTxBody $ balanceTx $ tx & #fee .~ feeCalc1

  -- Build the transaction one more time so that the tx.body file has the finalized transaction.
  -- Also set the witnesses since the GUI will need them to determine what actions can be taken
  -- on the tx.body file.
  let finalizedTx = balanceTx $ tx & #fee .~ fee
                                   & #witnesses .~ witnesses
                                   & #allWitnessesKnown .~ all (isJust . snd . unWitness) witnesses
  runCmd_ $ buildRawCmd paramsFile txBodyFile $ convertToTxBody finalizedTx

  -- Return the updated `TxBuilderModel`. Mark the model as built.
  return $ finalizedTx & #isBuilt .~ True

-- | Calculate the fee.
estimateTxFee :: Network -> ParamsFile -> TxBodyFile -> TxBody -> IO Lovelace
estimateTxFee network paramsFile txBodyFile tx@TxBody{inputs,outputs,witnesses} = do
    -- This builds the tx.body file and stores it in the tmp directory. 
    runCmd_ $ buildRawCmd paramsFile txBodyFile tx

    -- This uses the tx.body file to estimate the transaction fee. 
    fromJustOrAppError "Could not parse min fee." . fmap Lovelace . readMaybe =<<
      runCmd calcFeeCmd
  where
    calcFeeCmd :: String
    calcFeeCmd =
      toString $ (<> " | cut -d' ' -f1") $ unwords
        [ "cardano-cli transaction calculate-min-fee"
        , "--tx-body-file " <> toText txBodyFile
        , toNetworkFlag network
        , "--protocol-params-file " <> toText paramsFile
        , "--tx-in-count " <> show (length inputs)
        , "--tx-out-count " <> show (length outputs)
        , "--witness-count " <> show (length witnesses)
        ]

-- | Create the actual `cardano-cli transaction build-raw` command.
buildRawCmd :: ParamsFile -> TxBodyFile -> TxBody -> String
buildRawCmd paramsFile outFile tx = toString $ unwords
    [ "cardano-cli transaction build-raw"
    , unwords $ map inputField $ tx ^. #inputs
    , unwords $ map outputField $ tx ^. #outputs
    , "--protocol-params-file " <> toText paramsFile
    , "--fee " <> show (unLovelace $ tx ^. #fee)
    , "--out-file " <> toText outFile
    ]
  where
    inputField :: TxBodyInput -> Text
    inputField TxBodyInput{utxoRef} = unwords
      [ "--tx-in " <> display utxoRef
      ]

    outputField :: TxBodyOutput -> Text
    outputField TxBodyOutput{..} = unwords
      [ mconcat 
          [ "--tx-out "
          , show $ unwords
              [ toText paymentAddress
              , show (unLovelace lovelace) <> " lovelace"
              , unwords $ for nativeAssets $ \NativeAsset{..} ->
                  "+ " <> show quantity <> " " <> display policyId <> "." <> display tokenName
              ] 
          ]
      ]
