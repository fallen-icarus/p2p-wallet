{-# LANGUAGE TemplateHaskell #-}

module Main where

import Monomer
import Monomer.Lens qualified as L
import Data.Either qualified as Either
import Data.FileEmbed (embedFile)
import Data.Time.Clock.POSIX qualified as Time
import Codec.BMP (parseBMP,writeBMP)
import System.FilePath ((</>), (<.>))

import P2PWallet.Actions.Database
import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.EventHandler
import P2PWallet.GUI.UIBuilder
import P2PWallet.Prelude

-- | The icon to use for when the app is open. It must be exported to a file for monomer to be able
-- to use it. It is compiled into the executable so that the icon file doesn't need to be installed,
-- too.
cardanoIcon :: ByteString
cardanoIcon = $(embedFile "./assets/icons/cardano.bmp")

main :: IO ()
main = do
    -- The cardano icon will be exported to the temporary directory.
    tmpDir <- getTemporaryDirectory
    let iconFilePath = tmpDir </> "cardano" <.> "bmp"

    -- Export the cardano icon.
    exportStatus <- case parseBMP $ toLazy cardanoIcon of
      Right bmp -> writeBMP iconFilePath bmp >> return (Right ())
      Left _ -> return $ Left "Couldn't parse cardano icon"
    
    -- The db file is located in the user's $XDG_DATA_HOME directory.
    dbFilePath <- getDatabasePath
    initializationStatus <- initializeDatabase dbFilePath

    -- Get the user's current time zone.
    timeZone <- getCurrentTimeZone

    -- Get the current date for the user.
    currentDate <- getCurrentDay timeZone

    -- Get the current time for the user.
    currentTime <- Time.getPOSIXTime

    let startupErrors = Either.lefts
          [ exportStatus
          , initializationStatus
          ]
        thirtyDaysAgo = addDays (-30) currentDate
        initModel = 
          def & #databaseFile .~ dbFilePath -- Use the full filepath for to the database.
              & #config % #timeZone .~ timeZone
              & #config % #currentDay .~ currentDate
              & #config % #currentTime .~ currentTime
              & #homeModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #dexModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #lendingModel % #borrowModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #lendingModel % #lendModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #optionsModel % #writerModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #aftermarketModel % #buyerModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
              & #aftermarketModel % #sellerModel % #txFilterModel % #dateRange % _1 ?~ thirtyDaysAgo
    startApp initModel handleEvent buildUI $ appCfg iconFilePath $ case startupErrors of
      [] -> AppInit
      (firstError : _) -> Alert firstError

  where
    appCfg :: FilePath -> AppEvent -> [AppConfig s AppEvent]
    appCfg iconFilePath x =
      [ appWindowTitle "P2P-DeFi Wallet"
      , appTheme customDarkTheme
      , appFontDefMem "Regular" $(embedFile "./assets/fonts/Roboto-Regular.ttf")
      , appFontDefMem "Medium" $(embedFile "./assets/fonts/Roboto-Medium.ttf")
      , appFontDefMem "Bold" $(embedFile "./assets/fonts/Roboto-Bold.ttf")
      , appFontDefMem "Italics" $(embedFile "./assets/fonts/Roboto-Italic.ttf")
      , appFontDefMem "Remix" $(embedFile "./assets/fonts/remixicon.ttf")
      , appWindowIcon $ toText iconFilePath
      , appInitEvent x
      -- The scaling is weird for some monitor resolutions so the auto-scaling is disabled.
      , appWindowState MainWindowMaximized
      , appDisableAutoScale True
      , appScaleFactor 2
      ]

    customDarkTheme :: Theme
    customDarkTheme =
      -- This is needed to change the background color of alerts.
      darkTheme & mergeThemeStyle L.dialogFrameStyle [bgColor customGray3]
                & mergeThemeStyle L.emptyOverlayStyle [bgColor $ black & #a .~ 0.4]
                & setThemeValue L.scrollBarWidth 8
                & setThemeValue L.scrollThumbWidth 5
                -- & mergeThemeStyle L.emptyOverlayStyle [bgColor $ customGray1 & #a .~ 0.8]
