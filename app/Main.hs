{-# LANGUAGE TemplateHaskell #-}

module Main where

import Monomer
import Monomer.Lens qualified as L
import Data.FileEmbed (embedFile)

import P2PWallet.Actions.BackupFiles
import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.GUI.EventHandler
import P2PWallet.GUI.UIBuilder
import P2PWallet.Prelude

main :: IO ()
main = do
    handle handleError $ do 
      (cfg,profiles) <- loadFromBackups
      let initModel = 
            def & config .~ cfg -- Use the saved configs.
                & knownProfiles .~ profiles -- Set the known profiles to choose from upon startup.
      startApp initModel handleEvent buildUI $ appCfg AppInit

  where
    appCfg :: AppEvent -> [AppConfig AppEvent]
    appCfg x =
      [ appWindowTitle "P2P-DeFi Wallet"
      , appTheme customDarkTheme
      , appFontDefMem "Regular" $(embedFile "./assets/fonts/Roboto-Regular.ttf")
      , appFontDefMem "Medium" $(embedFile "./assets/fonts/Roboto-Medium.ttf")
      , appFontDefMem "Bold" $(embedFile "./assets/fonts/Roboto-Bold.ttf")
      , appFontDefMem "Italics" $(embedFile "./assets/fonts/Roboto-Italic.ttf")
      , appFontDefMem "Remix" $(embedFile "./assets/fonts/remixicon.ttf")
      , appInitEvent x
      , appWindowState $ MainWindowNormal (1350,850)
      ]

    handleError :: AppError -> IO ()
    handleError (AppError err) = startApp def handleEvent buildUI $ appCfg $ Alert err

    customDarkTheme :: Theme
    customDarkTheme = darkTheme
      & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"
