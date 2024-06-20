{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.BuildTxBody
  (
    buildTxBody
  ) where

import System.FilePath ((</>), (<.>))

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Files
import P2PWallet.Plutus
import P2PWallet.Prelude

buildTxBody :: Network -> TxBuilderModel -> IO TxBuilderModel
buildTxBody network tx = do
  -- Check if the transaction can be built without errors. This will throw an AppError if it
  -- cannot.
  canBeBuilt tx

  -- Get the file names since cardano-cli works with files.
  tmpDir <- getTemporaryDirectory
  let paramsFile = ParamsFile $ tmpDir </> "params" <.> "json"
      txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"

  -- Get the current parameters, and write them to a file for cardano-cli to use.
  runGetParams network >>= fromRightOrAppError >>= writeFileBS (toString paramsFile)

  -- Update the transaction fee. The changeOutput is already rebalanced to account for the fee.
  newTx <- estimateTxFee network paramsFile txBodyFile tx

  -- Build the transaction one more time so that the tx.body file has the finalized transaction.
  void $ runCmd $ buildRawCmd paramsFile txBodyFile newTx

  -- Return the final transaction after setting `isBuilt` to True.
  return $ newTx & #isBuilt .~ True

-- Since cardano-cli transaction build-raw can throw confusing error messages if certain
-- pieces are missing from the transaction, this check validates those cases will not occur.
-- It will throw an `AppError` with a more user friendly error message if cardano-cli 
-- transaction build-raw is likely to throw an error. The UI should catch these errors sooner but
-- the redundant checks can't hurt and enable building to be tested without a UI.
canBeBuilt :: TxBuilderModel -> IO ()
canBeBuilt TxBuilderModel{userInputs,changeOutput}
  -- The tx must have inputs.
  | userInputs == [] = throwIO $ AppError "No inputs specified."
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

-- | Calculate the fee twice. The first time is to get the initial estimate and the second
-- time is to account for adding the new fee to the transaction.
estimateTxFee 
  :: Network 
  -> ParamsFile 
  -> TxBodyFile 
  -> TxBuilderModel 
  -> IO TxBuilderModel
estimateTxFee network paramsFile txBodyFile startTx = estimate startTx >>= estimate
  where
    estimate :: TxBuilderModel -> IO TxBuilderModel
    estimate tx@TxBuilderModel{userInputs,userOutputs} = do
      -- Get all key witnesses that must sign this transaction.
      witnesses <- fromRightOrAppError $ getRequiredWitnesses tx

      -- This builds the tx.body file and stores it in the tmp directory. 
      void $ runCmd $ buildRawCmd paramsFile txBodyFile tx

      -- This uses the tx.body file to estimate the transaction fee. It will update
      -- the fee inside the `TxBuilderModel`.
      minFee <- fromJustOrAppError "Could not parse min fee." . fmap Lovelace . readMaybe =<<
        runCmd (calcFeeCmd userInputs userOutputs witnesses)

      -- Add the fee to the transaction and rebalance the change output.
      return $ balanceTx $ tx & #fee .~ minFee

    calcFeeCmd 
      :: [(Int,UserInput)] 
      -> [(Int,UserOutput)] 
      -> [Witness]
      -> String
    calcFeeCmd inputs outputs wits =
      toString $ (<> " | cut -d' ' -f1") $ unwords
        [ "cardano-cli transaction calculate-min-fee"
        , "--tx-body-file " <> toText txBodyFile
        , toNetworkFlag network
        , "--protocol-params-file " <> toText paramsFile
        , "--tx-in-count " <> show (length inputs)
        , "--tx-out-count " <> show (length outputs)
        , "--witness-count " <> show 
            (length $ ordNubOn fst $ map (view #witness) wits)
        ]

buildRawCmd :: ParamsFile -> TxBodyFile -> TxBuilderModel -> String
buildRawCmd paramsFile outFile tx = toString $ unwords
    [ "cardano-cli transaction build-raw"
    , unwords $ map inputField $ tx ^. #userInputs
    , unwords $ map outputField $ tx ^. #userOutputs
    , changeField $ fromMaybe def $ tx ^. #changeOutput
    , "--protocol-params-file " <> toText paramsFile
    , "--fee " <> show (unLovelace $ tx ^. #fee)
    , "--out-file " <> toText outFile
    ]
  where
    inputField :: (Int,UserInput) -> Text
    inputField (_,input) = unwords
      [ "--tx-in " <> (showTxOutRef $ input ^. #utxoRef)
      ]

    outputField :: (Int,UserOutput) -> Text
    outputField (_,output) = unwords
      [ mconcat 
          [ "--tx-out "
          , show $ unwords
              [ toText $ output ^. #paymentAddress
              , show (unLovelace $ output ^. #lovelace) <> " lovelace"
              , unwords $ flip map (output ^. #nativeAssets) $ \NativeAsset{..} ->
                  "+ " <> show quantity <> " " <> policyId <> "." <> tokenName
              ] 
          ]
      ]

    -- The fee has already been subtracted from the change output.
    changeField :: ChangeOutput -> Text
    changeField output = unwords
      [ mconcat 
          [ "--tx-out "
          , show $ unwords
              [ toText $ output ^. #paymentAddress
              , show (unLovelace $ output ^. #lovelace) <> " lovelace"
              , unwords $ flip map (output ^. #nativeAssets) $ \NativeAsset{..} ->
                  "+ " <> show quantity <> " " <> policyId <> "." <> tokenName
              ] 
          ]
      ]
