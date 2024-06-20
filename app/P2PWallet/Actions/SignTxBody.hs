{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.SignTxBody
  (
    signTxBody
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Files
import P2PWallet.Prelude

-- | Sign a transaction. Signing assumes the same hardware wallet seed phrase manages all
-- relevant keys for the transaction. It returns the filepath to the newly signed file.
signTxBody :: Network -> TxBuilderModel -> IO SignedTxFile
signTxBody network' tx = do
  tmpDir <- getTemporaryDirectory
  let txBodyFile = TxBodyFile $ tmpDir </> "tx" <.> "body"
      transformedTxFile = TransformedTxFile $ tmpDir </> "tx" <.> "transformed"
      signedFile = SignedTxFile $ tmpDir </> "tx" <.> "signed"

  -- Convert the tx.body file to the proper form for using the hardware wallet.
  void $ runCmd $ transformTxBodyCmd txBodyFile transformedTxFile

  -- Get all required witnesses.
  wits <- fromRightOrAppError $ getRequiredWitnesses tx

  -- Export all required pubkeys. Return the file names used for the hwsKeyFile. The file names
  -- will be prefixed by the respective key hashes. 
  witFiles <- forM wits $ 
    \(Witness (pkh,mPath)) -> do
      let hash = show pkh 
      path <- fromJustOrAppError ("Unknown pubkey required: " <> toText hash) mPath
      hwsFile <- snd <$> exportHwKeyFiles (Just hash) path
      return (hwsFile, WitnessFile $ tmpDir </> hash <.> "witness")
  
  -- Create the required witness files.
  void $ runCmd $ witnessTxCmd network' transformedTxFile witFiles

  -- Assemble the witnesses to produce the final tx.signed file.
  void $ runCmd $ assembleWitnessesCmd transformedTxFile witFiles signedFile

  -- Return the filepath to the new tx.signed file.
  return signedFile

transformTxBodyCmd :: TxBodyFile -> TransformedTxFile -> String
transformTxBodyCmd txBodyFile transformedTxFile = 
    printf cmdTemplate (toString txBodyFile) (toString transformedTxFile)
  where
    cmdTemplate = "cardano-hw-cli transaction transform --tx-file %s --out-file %s" 

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
  -> [(HwSigningFile,WitnessFile)]
  -> String
witnessTxCmd network' transformedTxFile normalWits = 
  let (normalHwsFiles,witnessFiles) = unzip normalWits
  in String.unwords
    [ "cardano-hw-cli transaction witness"
    , printf "--tx-file %s" $ toString transformedTxFile
    , String.unwords $ flip map normalHwsFiles $ \hwsFile -> 
        "--hw-signing-file " <> toString hwsFile
    , toString $ toNetworkFlag network'
    , String.unwords $ flip map witnessFiles $ \witnessFile ->
        "--out-file " <> toString witnessFile
    ]
