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
      (cfg,prevWallets) <- loadFromBackups
      let initModel = 
            def & config .~ cfg -- Use the saved configs.
                & wallets .~ prevWallets -- Load the saved wallets.
                & homeModel . selectedWallet .~ -- Set the target wallet to the first one.
                    fromMaybe def (maybeHead $ prevWallets ^. paymentWallets)
                & delegationModel . selectedWallet .~ -- Set the target wallet to the first one.
                    fromMaybe def (maybeHead $ prevWallets ^. stakeWallets)
          initEvent = 
            if prevWallets == def 
            then AppInit -- No tracked wallets.
            else SyncWallets StartSync -- Already has wallets.
      startApp initModel handleEvent buildUI $ appCfg initEvent

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
