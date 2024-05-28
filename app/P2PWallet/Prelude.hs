{-# OPTIONS_GHC -Wno-orphans #-}

module P2PWallet.Prelude
  ( -- * Text
    T.replace

    -- * Time
  , Time.POSIXTime
  , Time.TimeZone
  , showLocalTime
  , showLocalDate
  , Time.getCurrentTimeZone

    -- * Defaults
  , Default.Default(..)

    -- * Decimal
  , Decimal
  , realFracToDecimal
  
    -- * Directories
  , getTemporaryDirectory

    -- * Miscelleneous Functions
  , showValue
  , maybeHead

    -- * Lens Helpers
  , boolLens
  , maybeLens

  -- * Exceptions
  , throwIO
  , catch
  , handle

    -- * Other useful re-exports
  , module Relude
  , module Optics
  , Printf.printf
  ) where

import Relude hiding (uncons)
import System.Directory qualified as Dir
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Default qualified as Default
import Data.Decimal (Decimal,realFracToDecimal)
import Text.Printf qualified as Printf
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson qualified as Aeson
import Monomer.Core.FromFractional(FromFractional(..))
import Control.Exception (throwIO,catch,handle)
import Optics

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

showLocalDate :: Time.TimeZone -> Time.POSIXTime -> Text
showLocalDate zone t = 
    toText $ Time.formatTime Time.defaultTimeLocale formatter localTime
  where
    utcTime = Time.posixSecondsToUTCTime t
    localTime = Time.utcToLocalTime zone utcTime
    formatter = "%b %d, %0Y"

showLocalTime :: Time.TimeZone -> Time.POSIXTime -> Text
showLocalTime zone t = 
    toText $ Time.formatTime Time.defaultTimeLocale formatter localTime
  where
    utcTime = Time.posixSecondsToUTCTime t
    localTime = Time.utcToLocalTime zone utcTime
    formatter = "%I:%M %p"

-------------------------------------------------
-- Lens Helpers
-------------------------------------------------
-- | A lens that interprets a `Maybe a` as True or False. This is usefull for widgets
-- that depend on a `Bool`.
boolLens :: a -> Lens' s (Maybe a) -> Lens' s Bool
boolLens def' targetLens = 
  lens (\m -> isJust $ m ^. targetLens)
       (\m b -> m & targetLens .~ if b then Just def' else Nothing)

maybeLens :: a -> Lens' s (Maybe a) -> Lens' s a
maybeLens def' targetLens = 
  lens (\m -> fromMaybe def' $ m ^. targetLens)
       (\m t -> m & targetLens .~ Just t)

-- -- | A lens into a sum type `a` with a default choice of type `b`. The unWrapper `(a -> b)`
-- -- must cover the cases where the other data constructors are present. This is useful for
-- -- editing the inner `Text` for types like `CertificateAction`.
-- sumsOfProductsLens :: (a -> b) -> (b -> a) -> A_Lens s a -> A_Lens s b
-- sumsOfProductsLens unWrapper wrapper targetLens =
--   lens (\m -> unWrapper $ m ^# targetLens)
--        (\m t -> m & targetLens #~ wrapper t)

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
