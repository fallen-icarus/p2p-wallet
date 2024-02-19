{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.BuildTxBody
  ( 
    buildTxBody
  ) where

import System.FilePath ((</>), (<.>))
import Data.List (partition)

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Files
import P2PWallet.Data.Lens hiding (info,network)
import P2PWallet.Data.Plutus
import P2PWallet.Prelude

-- | Lookup all untracted UTxO inputs. Then try to balance and build a transaction using 
-- `cardano-cli build-raw`. It will automatically set execution budgets and tx fee. A dedicated 
-- change output must be used so that the fee does not need to be taken from any of the other 
-- outputs.
buildTxBody :: Network -> TxBuilderModel -> IO TxBuilderModel
buildTxBody network tx@TxBuilderModel{_inputs,_changeOutput,_certificates} = do
    -- Check if the transaction can be built without errors. This will throw an AppError if it
    -- cannot.
    canBeBuilt tx

    -- Lookup the information for any unknown UTxOs and update the `TxBuilderModel` with them.
    -- This is necessary to properly balance the transaction.
    updateTx <- flip (set inputs) tx <$> lookupUnknownUTxOs

    tmpDir <- getTemporaryDirectory
    let paramsFile = ParamsFile $ tmpDir </> "params" <.> "json"
        txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"
        params = case network of
          Mainnet -> mainnetParams
          Testnet -> preprodParams

    -- Export the param file so that cardano-cli can use it.
    writeFileBS (toString paramsFile) params

    -- Build the certificate files so they are available in the tmp directory. Registrations must
    -- appear first in the transaction.
    certFiles <- mapM (buildCertificate tmpDir) $ 
      sortOn (view certificateAction . snd) _certificates

    -- Update the transaction fee. The changeOutput is already rebalanced to account for the
    -- fee.
    newTx <- estimateTxFee network paramsFile txBodyFile certFiles updateTx

    -- Build the transaction one more time so that the tx.body file has the finalized transaction.
    void $ runCmd $ buildRawCmd paramsFile txBodyFile certFiles newTx

    -- Return the balanced transaction.
    return newTx

  where
    lookupUnknownUTxOs :: IO [(Int,VerifiedInput)]
    lookupUnknownUTxOs = do
      -- All untracked inputs will have an empty address field.
      let (untrackedInputs,trackedInputs) = partition (\(_,i) -> i ^. paymentAddress == "") _inputs
          untrackedRefs = map (view utxoRef . snd) untrackedInputs

      -- Lookup the information for the untracked UTxOs. Don't submit an empty list to Koios.
      untrackedInputInfo <- 
        case untrackedRefs of
          [] -> return [] 
          _ -> runQueryUnknownUTxOInfo network untrackedRefs >>= fromRightOrAppError 

      -- Updated the information for the new inputs. They should already be in the proper order.
      let updatedInfo = 
            (flip . flip zipWith) untrackedInputs untrackedInputInfo $ \(idx,i) info ->
              (idx, updateVerifiedInput info i)

      -- Recreate the input list with the updated info.
      return $ sortOn fst $ updatedInfo <> trackedInputs

-- Since cardano-cli transaction build-raw can throw confusing error messages if certain
-- pieces are missing from the transaction, this check validates those cases will not occur.
-- It will throw an `AppError` with a more user friendly error message if cardano-cli 
-- transaction build-raw is likely to throw an error.
canBeBuilt :: TxBuilderModel -> IO ()
canBeBuilt TxBuilderModel{_inputs,_changeOutput}
  -- The tx must have inputs.
  | _inputs == [] = throwIO $ AppError "No inputs specified."
  -- A change address must be specified.
  | _changeOutput ^. paymentAddress == "" = throwIO $ AppError "Change address missing."
  -- The inputs must have more assets than the outputs.
  | moreOutputAssetsThanInputAssets = throwIO $ AppError "Outputs have more assets than inputs." 
  | otherwise = return ()
  where
    moreOutputAssetsThanInputAssets :: Bool
    moreOutputAssetsThanInputAssets = _changeOutput ^. lovelaces < 0 
                                   || any ((<0) . view quantity) (_changeOutput ^. nativeAssets)

-- | Calculate the fee twice. The first time is to get the initial estimate and the second
-- time is to account for adding the new fee to the transaction.
estimateTxFee 
  :: Network 
  -> ParamsFile 
  -> TxBodyFile 
  -> [CertificateFile]
  -> TxBuilderModel 
  -> IO TxBuilderModel
estimateTxFee network paramsFile txBodyFile certFiles startTx = 
    estimate startTx >>= estimate
  where
    estimate :: TxBuilderModel -> IO TxBuilderModel
    estimate tx = do
      witnesses <- fromRightOrAppError $ requiredWitnesses tx

      -- This builds the tx.body file and stores it in the tmp directory. 
      void (runCmd $ buildRawCmd paramsFile txBodyFile certFiles tx)

      -- This uses the tx.body file to estimate the transaction fee. It will update
      -- the fee inside the `TxBuilderModel`.
      minFee <- fromJustOrAppError "Could not parse min fee." . fmap Lovelace . readMaybe =<<
        runCmd (calcFeeCmd network paramsFile txBodyFile (tx ^. inputs) (tx ^. outputs) witnesses)

      -- Add the fee to the transaction and rebalance the change output.
      return $ balanceTx $ tx & txFee .~ minFee

buildRawCmd :: ParamsFile -> TxBodyFile -> [CertificateFile] -> TxBuilderModel -> String
buildRawCmd paramsFile outFile certFiles tx = toString $ unwords
    [ "cardano-cli transaction build-raw"
    , unwords $ map inputField $ tx ^. inputs
    , unwords $ map outputField $ tx ^. outputs
    , changeField $ tx ^. changeOutput
    , unwords $ flip map certFiles $ \certFile -> "--certificate-file " <> toText certFile
    , "--protocol-params-file " <> toText paramsFile
    , "--fee " <> show (unLovelace $ tx ^. txFee)
    , "--out-file " <> toText outFile
    ]
  where
    inputField :: (Int,VerifiedInput) -> Text
    inputField (_,input) = unwords
      [ "--tx-in " <> (showTxOutRef $ input ^. utxoRef)
      ]

    outputField :: (Int,VerifiedOutput) -> Text
    outputField (_,output) = unwords
      [ mconcat 
          [ "--tx-out "
          , show $ unwords
              [ toText $ output ^. paymentAddress
              , show (unLovelace $ output ^. lovelaces) <> " lovelace"
              , unwords $ flip map (output ^. nativeAssets) $ \asset ->
                  "+ " <> show (asset ^. quantity) <> " " <> (fullAssetName asset)
              ] 
          ]
      ]

    -- The fee has already been subtracted from the change output.
    changeField :: VerifiedChangeOutput -> Text
    changeField output = unwords
      [ mconcat 
          [ "--tx-out "
          , show $ unwords
              [ toText $ output ^. paymentAddress
              , show (unLovelace $ output ^. lovelaces) <> " lovelace"
              , unwords $ flip map (output ^. nativeAssets) $ \asset ->
                  "+ " <> show (asset ^. quantity) <> " " <> (fullAssetName asset)
              ] 
          ]
      ]

calcFeeCmd 
  :: Network
  -> ParamsFile 
  -> TxBodyFile 
  -> [(Int,VerifiedInput)] 
  -> [(Int,VerifiedOutput)] 
  -> ([NormalWitness],[RegistrationWitness])
  -> String
calcFeeCmd network paramsFile txBodyFile _inputs _outputs (normalWits,regWits) = 
  toString $ (<> " | cut -d' ' -f1") $ unwords
    [ "cardano-cli transaction calculate-min-fee"
    , "--tx-body-file " <> toText txBodyFile
    , toNetworkFlag network
    , "--protocol-params-file " <> toText paramsFile
    , "--tx-in-count " <> show (length _inputs)
    , "--tx-out-count " <> show (length _outputs)
    , "--witness-count " <> show 
        (length $ ordNubOn fst $ map (view witness) normalWits <> map (view witness) regWits)
    ]

-- | Build all required certificates and return the filepaths used. Since a transaction
-- can contain multiple certificates for a given stakeAddress, the index is used to
-- differentiate between each certificate.
buildCertificate :: FilePath -> (Int,VerifiedCertificate) -> IO CertificateFile
buildCertificate tmpDir (i,VerifiedCertificate{..}) = do
    let certFile = tmpDir </> (toString _stakeAddress <> "_" <> show i) <.> "cert"
    void $ runCmd $
      case _certificateAction of
        Registration -> toString $ unwords
          [ "cardano-cli stake-address registration-certificate"
          , "--stake-address " <> toText _stakeAddress
          , "--out-file " <> toText certFile
          ]
        Deregistration -> toString $ unwords
          [ "cardano-cli stake-address deregistration-certificate"
          , "--stake-address " <> toText _stakeAddress
          , "--out-file " <> toText certFile
          ]
        Delegation pool -> toString $ unwords
          [ "cardano-cli stake-address delegation-certificate"
          , "--stake-address " <> toText _stakeAddress
          , "--stake-pool-id " <> toText pool
          , "--out-file " <> toText certFile
          ]

    return $ CertificateFile certFile
