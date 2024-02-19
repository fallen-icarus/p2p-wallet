{-
 
Both the tracked wallets and the current configuration need to be backed up and loaded. The
config file is located in XDG_CONFIG_HOME and the tracked wallets files are located in 
XDG_DATA_HOME. You can find the relevant information [here](https://hackage.haskell.org/package/directory-1.3.8.3/docs/System-Directory.html#t:XdgDirectory).

Within the XDG_DATA_HOME directory, the tracked wallets are located in either the 
p2pWallet/testnet/ directory or the p2pWallet/mainnet/ directory. To figure out where to look,
the config file must be loaded first in order to get the target network.

-}
module P2PWallet.Actions.BackupFiles
  ( 
    loadFromBackups
  , backupWallets
  , backupConfig
  ) where

import System.FilePath ((</>), (<.>), takeDirectory)
import System.Directory qualified as Dir
import Data.Aeson (decode,encode)
import Data.Aeson.Encode.Pretty (encodePretty)

import P2PWallet.Actions.Utils
import P2PWallet.Data.App.Config
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Loading
-------------------------------------------------
-- | Load config file. If the config file does not exist, create a default one and return
-- the default `Config`. When writing to the config file, use pretty aeson encoding to make
-- working with it more human friendly.
loadConfig :: IO (Either Text Config)
loadConfig = do
  let configFile = "p2p-wallet" </> "config" <.> "json"
  filepath <- Dir.getXdgDirectory Dir.XdgConfig configFile

  -- Check if the file exists already.
  exists <- Dir.doesFileExist filepath

  if exists 
    then do
      let readErrorMessage err = unlines
            [ "Could not read the config file: " <> toText filepath
            , ""
            , show err
            ]
          decodeErrorMessage = unlines
            [ "Could not parse the config file: " <> toText filepath
            , ""
            , "Check the file for errors, or delete it and restart app."
            ]

      -- Try to read and decode the file.
      handle @SomeException (return . Left . readErrorMessage) $
        maybeToRight decodeErrorMessage . decode @Config <$> readFileLBS filepath

    else do
      -- Create any necessary parent directories.
      Dir.createDirectoryIfMissing True (takeDirectory filepath)

      -- Create a default config file.
      writeFileLBS filepath $ encodePretty @Config def

      -- Return the defaults.
      return $ Right def

-- | Load the tracked wallets from the proper subdirectory. If the file does not exist, return
-- the default `Wallets` (i.e., no tracked wallets).
loadWallets :: Network -> IO (Either Text Wallets)
loadWallets network = do
  let walletsFile = "p2p-wallet" </> toString network </> "wallets" <.> "json"
  filepath <- Dir.getXdgDirectory Dir.XdgData walletsFile

  -- Check if the file exists already.
  exists <- Dir.doesFileExist filepath

  if exists 
    then do
      let readErrorMessage err = unlines
            [ "Could not read the wallets file: " <> toText filepath
            , ""
            , show err
            ]
          decodeErrorMessage = unlines
            [ "Could not parse the wallets file: " <> toText filepath
            , ""
            , "Check the file for errors, or delete it and re-pair wallets."
            ]

      -- Try to read and decode the file.
      handle @SomeException (return . Left . readErrorMessage) $
        maybeToRight decodeErrorMessage . decode @Wallets <$> readFileLBS filepath

    else do
      -- Create any necessary parent directories.
      Dir.createDirectoryIfMissing True (takeDirectory filepath)

      -- Create a default config file.
      writeFileLBS filepath $ encode @Wallets def

      -- Return the defaults.
      return $ Right def

-- | Try to load the backups. Convert any errors to an `AppError`.
loadFromBackups :: IO (Config,Wallets)
loadFromBackups = do
  cfg@Config{_network} <- loadConfig >>= fromRightOrAppError
  wallets <- loadWallets _network >>= fromRightOrAppError
  return (cfg,wallets)

-------------------------------------------------
-- Saving
-------------------------------------------------
-- | Save the currently tracked wallets. The backup file should have already been created upon
-- initializing the app.
backupWallets :: Network -> Wallets -> IO ()
backupWallets network ws = do
  let walletsFile = "p2p-wallet" </> toString network </> "wallets" <.> "json"
  filepath <- Dir.getXdgDirectory Dir.XdgData walletsFile

  -- Write to the file.
  writeFileLBS filepath $ encode ws

-- | Save the configuration. The config file should have already been created upon
-- initializing the app.
backupConfig :: Config -> IO ()
backupConfig cfg = do
  let configFile = "p2p-wallet" </> "config" <.> "json"
  filepath <- Dir.getXdgDirectory Dir.XdgConfig configFile

  -- Write to the file.
  writeFileLBS filepath $ encodePretty cfg
