{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Main where

import Monomer
import Monomer.Lens qualified as L
import Data.FileEmbed (embedFile)

import System.FilePath ((</>), (<.>))
import System.Directory qualified as Dir

import P2PWallet.Actions.Database
import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.EventHandler
import P2PWallet.GUI.UIBuilder
import P2PWallet.Prelude

main :: IO ()
main = do
    -- The db file is located in the user's $XDG_DATA_HOME directory.
    let dbName = "p2p-wallet" </> "p2p-wallet" <.> "db"
    dbFilePath <- Dir.getXdgDirectory Dir.XdgData dbName

    whenLeftM_ (initializeDatabase dbFilePath) (throwIO . AppError)

    -- Get the user's current time zone.
    timeZone <- getCurrentTimeZone

    -- Get the current date for the user.
    currentDate <- getCurrentDay timeZone

    let thirtyDaysAgo = addDays (-30) currentDate
        initModel = 
          def & #databaseFile .~ dbFilePath -- Use the full filepath for to the database.
              & #config % #timeZone .~ timeZone
              & #config % #currentDay .~ currentDate
              & #homeModel % #txFilterModel % #dateRange % _1 .~ Just thirtyDaysAgo
    startApp initModel handleEvent buildUI $ appCfg AppInit

  where
    appCfg :: AppEvent -> [AppConfig s AppEvent]
    appCfg x =
      [ appWindowTitle "P2P-DeFi Wallet"
      , appTheme customDarkTheme
      , appFontDefMem "Regular" $(embedFile "./assets/fonts/Roboto-Regular.ttf")
      , appFontDefMem "Medium" $(embedFile "./assets/fonts/Roboto-Medium.ttf")
      , appFontDefMem "Bold" $(embedFile "./assets/fonts/Roboto-Bold.ttf")
      , appFontDefMem "Italics" $(embedFile "./assets/fonts/Roboto-Italic.ttf")
      , appFontDefMem "Remix" $(embedFile "./assets/fonts/remixicon.ttf")
      , appInitEvent x
      -- , appWindowState $ MainWindowNormal (700,700)
      ]

    customDarkTheme :: Theme
    customDarkTheme =
      -- This is needed to change the background color of alerts.
      darkTheme & mergeThemeStyle L.dialogFrameStyle [bgColor customGray3]
                & mergeThemeStyle L.emptyOverlayStyle [bgColor $ black & #a .~ 0.4]
                -- & mergeThemeStyle L.emptyOverlayStyle [bgColor $ customGray1 & #a .~ 0.8]
