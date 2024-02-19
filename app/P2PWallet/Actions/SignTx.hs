module P2PWallet.Actions.SignTx
  ( 
    signTx
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Files
import P2PWallet.Prelude

-- | Sign a transaction. Signing assumes the same hardware wallet seed phrase manages all
-- relevant keys for the transaction. It returns the filepath to the newly signed file.
signTx :: Network -> TxBuilderModel -> IO SignedTxFile
signTx network' tx = do
  tmpDir <- getTemporaryDirectory
  let txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"
      transformedTxFile = TransformedTxFile $ tmpDir </> "tx" <.> "transformed"
      signedFile = SignedTxFile $ tmpDir </> "tx" <.> "signed"

  -- Convert the tx.body file to the proper form for using the hardware wallet.
  void $ runCmd $ transformTxBodyCmd txBodyFile transformedTxFile

  -- Get all required witnesses.
  (normalWits,registrationWits) <- fromRightOrAppError $ requiredWitnesses tx

  -- Export all required pubkeys. Return the file names used for the hwsKeyFile. The file names
  -- will be prefixed by the respective key hashes. Also returned the output witness filepaths
  -- prefixed with the corresponding pubkey hash.
  normalWitFiles <- forM normalWits $ 
    \(NormalWitness (pkh,mPath)) -> do
      let hash = show pkh 
      path <- fromJustOrAppError ("Unknown pubkey required: " <> toText hash) mPath
      hwsFile <- snd <$> exportHwKeyFiles (Just hash) path
      return (hwsFile, WitnessFile $ tmpDir </> hash <.> "witness")
  
  -- Export all required pubkeys. Return the file names used for the hwsKeyFile. The file names
  -- will be prefixed by the respective key hashes. 
  regHwsFiles <- forM registrationWits $ \(RegistrationWitness (pkh,mPath)) -> do
    path <- fromJustOrAppError ("Unknown pubkey required: " <> show pkh) mPath
    snd <$> exportHwKeyFiles (Just $ show pkh) path
  
  -- Create the required witness files. Registation witnesses do not create a witness file.
  void $ runCmd $ witnessTxCmd network' transformedTxFile regHwsFiles normalWitFiles

  -- Assemble the witnesses to produce the final tx.signed file.
  void $ runCmd $ assembleWitnessesCmd transformedTxFile normalWitFiles signedFile

  -- Return the filepath to the new tx.signed file.
  return signedFile

transformTxBodyCmd :: TxBodyFile -> TransformedTxFile -> String
transformTxBodyCmd txBodyFile transformedTxFile =
  printf 
    "cardano-hw-cli transaction transform --tx-file %s --out-file %s" 
    (toString txBodyFile)
    (toString transformedTxFile)

assembleWitnessesCmd 
  :: TransformedTxFile 
  -> [(HwSigningFile,WitnessFile)] 
  -> SignedTxFile 
  -> String
assembleWitnessesCmd transformedTxFile witnessFiles signedFile =
  String.unwords 
    [ "cardano-cli transaction assemble"
    , printf "--tx-body-file %s" $ toString transformedTxFile
    , String.unwords $ map (printf "--witness-file %s" . toString . snd) witnessFiles
    , printf "--out-file %s" $ toString signedFile
    ]

witnessTxCmd 
  :: Network 
  -> TransformedTxFile 
  -> [HwSigningFile] -- Witnesses that do not produce an output file.
  -> [(HwSigningFile,WitnessFile)] -- Witnesses that do produce an output file.
  -> String
witnessTxCmd network' transformedTxFile regHwsFiles normalWits = 
  let (normalHwsFiles,witnessFiles) = unzip normalWits
  in String.unwords
    [ "cardano-hw-cli transaction witness"
    , printf "--tx-file %s" $ toString transformedTxFile
    , String.unwords $ flip map (regHwsFiles <> normalHwsFiles) $ \hwsFile -> 
        "--hw-signing-file " <> toString hwsFile
    , toString $ toNetworkFlag network'
    , String.unwords $ flip map witnessFiles $ \witnessFile ->
        "--out-file " <> toString witnessFile
    ]
