module P2PWallet.Actions.AssembleWitnesses
  (
    assembleWitnesses
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal.Files
import P2PWallet.Prelude

-- | Assemble a list of witness files to produce the signed file. It returns the filepath to the
-- signed transaction file.
assembleWitnesses :: [KeyWitnessFile] -> IO SignedTxFile
assembleWitnesses witnessFiles = do
  tmpDir <- getTemporaryDirectory
  let transformedTxFile = TransformedTxFile $ tmpDir </> "tx" <.> "body"
      signedFile = SignedTxFile $ tmpDir </> "tx" <.> "signed"

  -- Assemble the witnesses to produce the final tx.signed file.
  runCmd_ $ assembleWitnessesCmd transformedTxFile witnessFiles signedFile

  -- Return the filepath to the new tx.signed file.
  return signedFile

-- | The command to assemble a list of witnesses and produce a tx.signed file.
assembleWitnessesCmd :: TransformedTxFile -> [KeyWitnessFile] -> SignedTxFile -> String
assembleWitnessesCmd transformedTxFile witnessFiles signedFile =
  String.unwords 
    [ "cardano-cli conway transaction assemble"
    , "--tx-body-file " <> toString transformedTxFile
    , String.unwords $ map (printf "--witness-file %s" . toString) witnessFiles
    , "--out-file " <> toString signedFile
    ]

