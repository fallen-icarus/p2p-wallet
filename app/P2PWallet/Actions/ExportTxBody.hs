module P2PWallet.Actions.ExportTxBody
  (
    exportTxBody
  ) where

import System.FilePath ((</>), (<.>), takeFileName)
import System.Directory (getHomeDirectory,copyFile,createDirectoryIfMissing)

import P2PWallet.Data.Core.Internal.Files
import P2PWallet.Prelude

-- | Export the transaction body file and any witness files to either the user's configured 
-- export directory or the user's home directory.
exportTxBody :: [KeyWitnessFile] -> IO FilePath
exportTxBody witnessFiles = do
  destinationDir <- getHomeDirectory
  tmpDir <- getTemporaryDirectory
  let appDir = destinationDir </> "tx_files"
      tmpTxBody = tmpDir </> "tx" <.> "body"
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
