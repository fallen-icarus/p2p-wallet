module P2PWallet.Actions.ExportHwKey
  ( 
    exportHwKeyFiles
  , exportHwPubKeyHash
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.Utils
import P2PWallet.Prelude
import P2PWallet.Data.App
import P2PWallet.Data.Files
import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Data.Plutus

-- | Export the pubkey files with an optional name.
exportHwKeyFiles :: Maybe FilePath -> DerivationPath -> IO (PubKeyFile,HwSigningFile)
exportHwKeyFiles prefix key = do
  tmpDir <- getTemporaryDirectory
  let pubKeyFile = 
        PubKeyFile $ tmpDir </> fromMaybe "new_key_export" prefix <.> ".vkey"
      hwsKeyFile = 
        HwSigningFile $ tmpDir </> fromMaybe "new_key_export" prefix <.> ".hwsfile"

  void $ runCmd $ exportPubKeyCmd key pubKeyFile hwsKeyFile
  return (pubKeyFile,hwsKeyFile)

exportHwPubKeyHash :: DerivationPath -> IO PubKeyHash
exportHwPubKeyHash key = do
    (pubKeyFile,_) <- exportHwKeyFiles Nothing key

    hash <- toText <$> runCmd (printf hashPubKeyFileCmd $ toString pubKeyFile)
    
    either (throwIO . AppError) return $
      maybeToRight "Could not parse pubkey hash" $ readPubKeyHash hash

  where
    hashPubKeyFileCmd = case key of
      PaymentKeyPath _ -> "cardano-cli address key-hash --payment-verification-key-file %s"
      StakeKeyPath _ -> "cardano-cli stake-address key-hash --stake-verification-key-file %s"

exportPubKeyCmd :: DerivationPath -> PubKeyFile -> HwSigningFile -> String
exportPubKeyCmd key pubKeyFile hwsKeyFile =
  String.unwords
    [ "cardano-hw-cli address key-gen"
    , printf "--path %s" $ showDerivationPath key
    , printf "--verification-key-file %s" $ toString pubKeyFile
    , printf "--hw-signing-file %s" $ toString hwsKeyFile
    ]
