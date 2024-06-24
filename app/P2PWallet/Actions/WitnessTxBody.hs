module P2PWallet.Actions.WitnessTxBody
  (
    witnessTxBody
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-- | Witness a transaction using the hardware wallet. Witnesssing assumes the same hardware wallet
-- seed phrase manages all relevant keys for the transaction. It returns the filepath to the new
-- witness files.
witnessTxBody :: Network -> TxBuilderModel -> IO [WitnessFile]
witnessTxBody network TxBuilderModel{witnesses,isBuilt} = do
  -- The transaction must be built.
  unless isBuilt $ throwIO $ AppError "The transaction must be built first."

  -- Get the absolute filepaths.
  tmpDir <- getTemporaryDirectory
  let txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"
      transformedTxFile = TransformedTxFile $ tmpDir </> "tx" <.> "transformed"

  -- Convert the tx.body file to the proper CBOR format for using the hardware wallet.
  -- This step may not be necessary anymore but it is still part of the cardano-hw-cli
  -- documentation.
  runCmd_ $ transformTxBodyCmd txBodyFile transformedTxFile

  -- Export all required pubkeys. Return the file names used for the hwsKeyFile. The file names
  -- will be prefixed by the respective key hashes. Skip witnesses for unknown derivation paths
  -- since these may be for a watched wallet.
  (hwsFiles,witnessFiles) <- fmap (unzip . catMaybes) $ forM witnesses $ 
    \(Witness (pkh,mPath)) -> flip (maybe (return Nothing)) mPath $ \path -> do
        let hash = show pkh 
        hwsFile <- snd <$> exportHwKeyFiles (Just hash) path
        return $ Just (hwsFile, WitnessFile $ tmpDir </> hash <.> "witness")
  
  -- Create the required witness files.
  runCmd_ $ witnessTxCmd network transformedTxFile hwsFiles witnessFiles

  -- Return the filepaths for the new witness files.
  return witnessFiles

-- | The command to convert the transaction to the proper cbor format.
transformTxBodyCmd :: TxBodyFile -> TransformedTxFile -> String
transformTxBodyCmd txBodyFile transformedTxFile = 
    printf cmdTemplate (toString txBodyFile) (toString transformedTxFile)
  where
    cmdTemplate = "cardano-hw-cli transaction transform --tx-file %s --out-file %s" 

-- | The command to witness a transaction using the hardware wallet. It can be used to witness for
-- multiple hardware wallet keys as long as all keys are for the same `account_index`.
witnessTxCmd :: Network -> TransformedTxFile -> [HwSigningFile] -> [WitnessFile] -> String
witnessTxCmd network transformedTxFile hwsFiles witnessFiles = 
  String.unwords
    [ "cardano-hw-cli transaction witness"
    , "--tx-file " <> toString transformedTxFile
    , String.unwords $ for hwsFiles $ \hwsFile -> 
        "--hw-signing-file " <> toString hwsFile
    , toString $ toNetworkFlag network
    , String.unwords $ for witnessFiles $ \witnessFile ->
        "--out-file " <> toString witnessFile
    ]
