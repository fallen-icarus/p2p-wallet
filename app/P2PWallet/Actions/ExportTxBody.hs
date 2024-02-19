module P2PWallet.Actions.ExportTxBody
  ( 
    exportTxBody
  ) where

import System.FilePath ((</>), (<.>))

import P2PWallet.Data.Files
import P2PWallet.Actions.Utils
import P2PWallet.Prelude

-- | Export the transaction's body file to the specified filepath. The source tx.body file is 
-- assumed to be located in the tmp directory since that is where `buildTxBody` stores it.
exportTxBody :: TxBodyFile -> IO Text
exportTxBody destFile = do
  tmpDir <- getTemporaryDirectory
  let sourceFile = TxBodyFile $ tmpDir </> "tx" <.> "body"

  void $ runCmd $ exportTxBodyCmd sourceFile destFile
  return $ "Successfully exported to: " <> toText destFile

exportTxBodyCmd :: TxBodyFile -> TxBodyFile -> String
exportTxBodyCmd sourceFile destFile =
  printf "cp %s %s" (toString sourceFile) (toString destFile)
