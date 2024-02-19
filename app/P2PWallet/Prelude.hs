{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TemplateHaskell #-}

module P2PWallet.Prelude
  ( -- * Text
    T.replace

    -- * Time
  , Time.POSIXTime
  , showLocalTime

    -- * Defaults
  , Default.Default(..)

    -- * Control.Lens re-exports
  , Lens.view
  , Lens.isn't
  , Lens.non
  , Lens.at
  , Lens.to
  , Lens.ALens'
  , Lens.Lens'
  , Lens._1
  , Lens._2
  , Lens._3
  , Lens._4
  , Lens._Just
  , Lens.set
  , (^.)
  , (^#)
  , (^?)
  , (?~)
  , (.~)
  , (#~)

    -- * Decimal
  , Decimal
  , realFracToDecimal
  
    -- * Hardcoded Network Parameters
  , mainnetParams
  , preprodParams

    -- * Directories
  , getTemporaryDirectory

    -- * Miscelleneous Functions
  , showValue
  , maybeHead

  -- * Exceptions
  , throwIO
  , catch
  , handle

    -- * Other useful re-exports
  , module Relude
  , Printf.printf
  ) where

import Relude
import System.Directory qualified as Dir
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Default qualified as Default
import Data.Decimal (Decimal,realFracToDecimal)
import Control.Lens qualified as Lens
import Control.Lens ((^.), (^?), (^#), (.~), (?~), (#~)) -- For some reason, `Lens.^.` doesn't work.
import Text.Printf qualified as Printf
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson qualified as Aeson
import Monomer.Core.FromFractional(FromFractional(..))
import Control.Exception (throwIO,catch,handle)
import Data.FileEmbed (embedFile)

preprodParams :: ByteString
preprodParams = $(embedFile "assets/network-parameters/preprod-params.json")

mainnetParams :: ByteString
mainnetParams = $(embedFile "assets/network-parameters/mainnet-params.json")

showLocalTime :: String -> Time.POSIXTime -> Text
showLocalTime f t = toText
                  $ Time.formatTime Time.defaultTimeLocale f 
                  $ Time.posixSecondsToUTCTime t

showValue :: Aeson.Value -> Text
showValue = decodeUtf8 
          . toStrict 
          . Aeson.encodingToLazyByteString 
          . Aeson.value

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- | A custom version of `Dir.getTemporaryDirectory` so that all temporary files are organized
-- within a subfolder.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
  tmpDir <- (<> "/p2p-wallet") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing True tmpDir -- Create the subfolder if it doesn't exist yet.
  return tmpDir

-------------------------------------------------
-- Orphans
-------------------------------------------------
instance FromFractional Decimal where
  fromFractional = realToFrac

instance Aeson.FromJSON Decimal where
  parseJSON = Aeson.withScientific "Decimal" (maybe mzero return . readMaybe . show)

instance Printf.PrintfArg Decimal where
  formatArg x fmt | Printf.fmtChar (Printf.vFmt 'D' fmt) == 'D' =
    Printf.formatString (show x) (fmt { Printf.fmtChar = 's', Printf.fmtPrecision = Nothing })
  formatArg _ fmt = Printf.errorBadFormat $ Printf.fmtChar fmt
