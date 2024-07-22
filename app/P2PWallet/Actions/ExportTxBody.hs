module P2PWallet.Actions.ExportTxBody
  (
    exportTxBody
  ) where

import System.FilePath ((</>), (<.>), takeFileName)
import System.Directory (getModificationTime,copyFile,createDirectoryIfMissing)

import P2PWallet.Data.Core.Internal.Files
import P2PWallet.Prelude

-- | Export the transaction body file and any witness files to the specified directory. A subfolder
-- is created in case other files already exist in that directory.
exportTxBody :: TimeZone -> Text -> [KeyWitnessFile] -> IO FilePath
exportTxBody zone destinationDir witnessFiles = do
  tmpDir <- getTemporaryDirectory
  let tmpTxBody = tmpDir </> "tx" <.> "body"

  timestamp <- timeStampToFilePath zone <$> getModificationTime tmpTxBody
      
  let appDir = toString destinationDir </> timestamp
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
