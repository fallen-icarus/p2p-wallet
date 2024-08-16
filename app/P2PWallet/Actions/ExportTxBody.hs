module P2PWallet.Actions.ExportTxBody
  (
    exportTxBody
  ) where

import System.FilePath ((</>), (<.>), takeFileName)
import System.Directory (copyFile,createDirectoryIfMissing)

import P2PWallet.Data.Core.Internal.Files
import P2PWallet.Prelude

-- | Export the transaction body file and any witness files to the specified directory. 
exportTxBody :: Text -> [KeyWitnessFile] -> IO FilePath
exportTxBody destinationDir witnessFiles = do
  tmpDir <- getTemporaryDirectory
  let tmpTxBody = tmpDir </> "tx" <.> "body"

  let appDir = toString destinationDir
      txBodyDestination = appDir </> "tx" <.> "body"

  -- Create the app directory if missing.
  createDirectoryIfMissing True appDir

  -- Copy the tx.body file to the export destination.
  copyFile tmpTxBody txBodyDestination

  -- Copy the witness files to the export destination.
  forM_ witnessFiles $ \(KeyWitnessFile witnessFile) ->
    copyFile witnessFile $ appDir </> takeFileName witnessFile

  -- Return the destination directory so the user knows where to look.
  return appDir
