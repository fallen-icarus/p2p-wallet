module P2PWallet.Actions.ExportHwKey
  ( 
    exportHwKeyFiles
  , exportHwPubKeyHash
  ) where

import System.FilePath ((</>), (<.>))
import Data.String qualified as String

import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | Export the pubkey files with an optional name.
exportHwKeyFiles :: Maybe FilePath -> DerivationPath -> IO (PubKeyFile,HwSigningFile)
exportHwKeyFiles prefix key = do
  tmpDir <- getTemporaryDirectory
  let pubKeyFile = 
        PubKeyFile $ tmpDir </> fromMaybe "new_key_export" prefix <.> ".vkey"
      hwsKeyFile = 
        HwSigningFile $ tmpDir </> fromMaybe "new_key_export" prefix <.> ".hwsfile"

  runCmd_ $ exportPubKeyCmd key pubKeyFile hwsKeyFile
  return (pubKeyFile,hwsKeyFile)

exportHwPubKeyHash :: DerivationPath -> IO PubKeyHash
exportHwPubKeyHash key = do
    (pubKeyFile,_) <- exportHwKeyFiles Nothing key

    hash <- toText <$> runCmd (printf hashPubKeyFileCmd $ toString pubKeyFile)
    
    either (throwIO . AppError) return $
      maybeToRight "Could not parse pubkey hash" $ parsePubKeyHash hash

  where
    hashPubKeyFileCmd = case key of
      PaymentKeyPath _ _ -> "cardano-cli address key-hash --payment-verification-key-file %s"
      StakeKeyPath _ _ -> "cardano-cli stake-address key-hash --stake-verification-key-file %s"

exportPubKeyCmd :: DerivationPath -> PubKeyFile -> HwSigningFile -> String
exportPubKeyCmd key pubKeyFile hwsKeyFile =
  String.unwords
    [ "cardano-hw-cli address key-gen"
    , printf "--path %s" $ display key
    , printf "--verification-key-file %s" $ toString pubKeyFile
    , printf "--hw-signing-file %s" $ toString hwsKeyFile
    ]
