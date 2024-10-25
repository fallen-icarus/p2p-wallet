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
-- witness files. The tx.body file is assumed to have been already transformed.
witnessTxBody :: Network -> TxBuilderModel -> IO [KeyWitnessFile]
witnessTxBody network TxBuilderModel{keyWitnesses,isBuilt} = do
  -- The transaction must be built.
  unless isBuilt $ throwIO $ AppError "The transaction must be built first."

  -- Get the absolute filepaths.
  tmpDir <- getTemporaryDirectory
  let transformedTxFile = TransformedTxFile $ tmpDir </> "tx" <.> "body"

  -- All key witnesses must use the same derivation type.
  let mDerType = maybeHead keyWitnesses >>= (view _3 . unKeyWitness >=> fst)

  -- Export all required pubkeys. Return the file names used for the hwsKeyFile. The file names
  -- will be prefixed by the respective key hashes. Skip witnesses for unknown derivation paths
  -- since these may be for a watched wallet.
  (hwsFiles,witnessFiles) <- fmap (unzip . catMaybes) $ forM keyWitnesses $ 
    \(KeyWitness (_,pkh,mPath)) -> flip (maybe (return Nothing)) mPath $ \path -> do
        let hash = show pkh 
        hwsFile <- snd <$> exportHwKeyFiles (Just hash) path
        return $ Just (hwsFile, KeyWitnessFile $ tmpDir </> hash <.> "witness")
  
  -- Create the required witness files.
  runCmd_ $ witnessTxCmd network transformedTxFile hwsFiles witnessFiles mDerType

  -- Return the filepaths for the new witness files.
  return witnessFiles

-- | The command to witness a transaction using the hardware wallet. It can be used to witness for
-- multiple hardware wallet keys as long as all keys are for the same `account_index` and derivation
-- type.
witnessTxCmd 
  :: Network 
  -> TransformedTxFile 
  -> [HwSigningFile] 
  -> [KeyWitnessFile] 
  -> Maybe DerivationType
  -> String
witnessTxCmd network transformedTxFile hwsFiles witnessFiles mDerType = 
  String.unwords
    [ "cardano-hw-cli transaction witness"
    , "--tx-file " <> toString transformedTxFile
    , String.unwords $ for hwsFiles $ \hwsFile -> 
        "--hw-signing-file " <> toString hwsFile
    , toString $ toNetworkFlag network
    , String.unwords $ for witnessFiles $ \witnessFile ->
        "--out-file " <> toString witnessFile
    , maybe "" (printf "--derivation-type %s" . showDerivationTypeForCLI) mDerType
    ]
