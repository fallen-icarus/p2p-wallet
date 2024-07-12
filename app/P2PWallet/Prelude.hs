{-# OPTIONS_GHC -Wno-orphans #-}

module P2PWallet.Prelude
  ( -- * Text
    T.replace

    -- * Time
  , Time.Day(..)
  , beginningOfDay
  , endOfDay
  , localTimeToPosixTime
  , Time.POSIXTime
  , Time.TimeZone
  , showLocalTime
  , showLocalDate
  , Time.getCurrentTimeZone
  , getCurrentDay
  , Time.addDays

    -- * Defaults
  , Default.Default(..)

    -- * Decimal
  , Decimal
  , realFracToDecimal
  
    -- * Directories
  , getTemporaryDirectory

    -- * Miscelleneous Functions
  , showValue
  , valueAsByteString
  , maybeHead
  , groupInto
  , mkScaleFactor
  , formatQuantity
  , unFormatQuantity
  , for

    -- * Lens Helpers
  , boolLens
  , maybeLens

    -- * Rational Parsers and Printers
  , parseDecimal
  , parsePercentage
  , displayPercentage

    -- * Exceptions
  , throwIO
  , catch
  , handle

  -- Display Class
  , Display(..)

    -- * Other useful re-exports
  , module Relude
  , module Optics
  , Printf.printf
  ) where

import Relude hiding (uncons)
import Relude.Unsafe qualified as Unsafe
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
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.Internal (Field(..))

showValue :: Aeson.Value -> Text
showValue = decodeUtf8 . valueAsByteString

valueAsByteString :: Aeson.Value -> ByteString
valueAsByteString = toStrict . Aeson.encodingToLazyByteString . Aeson.value

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

-- | Break a list into sublists of the specified length.
groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = 
  take n xs : groupInto n (drop n xs)

-- | The factor to use when accounting for the specified decimal places.
mkScaleFactor :: Word8 -> Rational
mkScaleFactor decimal = 
  Unsafe.read @Rational $ "1" <> replicate (fromIntegral decimal) '0' <> " % 1"

-- | Format an integer to the specified decimal places by using the scale factor. This is useful
-- for showing asset values using the desired number of decimal places.
formatQuantity :: Word8 -> Integer -> Decimal
formatQuantity decimal quantity = 
  realFracToDecimal decimal $ toRational quantity / mkScaleFactor decimal

-- | Convert a `Decimal` back to the specified `Integer` by using the scale factor.
unFormatQuantity :: Word8 -> Decimal -> Integer
unFormatQuantity decimal quantity = round $ toRational quantity * mkScaleFactor decimal

-- | A custom `for` function since the one in `Data.Traversable` does not work for some reason.
for :: [a] -> (a -> b) -> [b]
for = flip map

-------------------------------------------------
-- Time
-------------------------------------------------
-- | Convert `Time.POSIXTime` to the user's local date. Formatting: "Feb 13, 2024"
showLocalDate :: Time.TimeZone -> Time.POSIXTime -> Text
showLocalDate zone t = 
    toText $ Time.formatTime Time.defaultTimeLocale formatter localTime
  where
    utcTime = Time.posixSecondsToUTCTime t
    localTime = Time.utcToLocalTime zone utcTime
    formatter = "%b %d, %0Y"

-- | Convert `Time.POSIXTime` to the user's local time. Formatting: "1:13 pm"
showLocalTime :: Time.TimeZone -> Time.POSIXTime -> Text
showLocalTime zone t = 
    toText $ Time.formatTime Time.defaultTimeLocale formatter localTime
  where
    utcTime = Time.posixSecondsToUTCTime t
    localTime = Time.utcToLocalTime zone utcTime
    formatter = "%I:%M %p"

beginningOfDay :: Time.Day -> Time.LocalTime
beginningOfDay day = Time.LocalTime day Time.midnight

endOfDay :: Time.Day -> Time.LocalTime
endOfDay day = Time.LocalTime (Time.addDays 1 day) Time.midnight

localTimeToPosixTime :: Time.TimeZone -> Time.LocalTime -> Time.POSIXTime
localTimeToPosixTime timeZone localTime = 
  Time.utcTimeToPOSIXSeconds $ Time.localTimeToUTC timeZone localTime

getCurrentDay :: Time.TimeZone -> IO Time.Day
getCurrentDay timeZone = 
  Time.localDay . Time.utcToLocalTime timeZone <$> Time.getCurrentTime

-------------------------------------------------
-- Lens Helpers
-------------------------------------------------
-- | A lens that interprets a `Maybe a` as True or False. This is usefull for widgets
-- that depend on a `Bool`, but the internal state of the App uses Maybe.
boolLens :: a -> Lens' s (Maybe a) -> Lens' s Bool
boolLens def' targetLens = 
  lens (\m -> isJust $ m ^. targetLens)
       (\m b -> m & targetLens .~ if b then Just def' else Nothing)

-- | A lens that can handle a type inside a Maybe. In the case of `Nothing`, a default value for
-- `a` will be used.
maybeLens :: a -> Lens' s (Maybe a) -> Lens' s a
maybeLens def' targetLens = 
  lens (\m -> fromMaybe def' $ m ^. targetLens)
       (\m t -> m & targetLens ?~ t)

-------------------------------------------------
-- Rational Parsers
-------------------------------------------------
-- | Parse a `Rational` entered as a decimal.
parseDecimal :: Text -> Maybe Rational
parseDecimal = fmap toRational . readMaybe @Decimal . T.unpack

-- | Parse a "12.5%" to a `Rational`.
parsePercentage :: Text -> Maybe Rational
parsePercentage = fmap (toRational . (/100)) 
                . readMaybe @Double 
                . T.unpack 

-- | Show a `Rational` as a percentage. This shows four decimal places.
displayPercentage :: Rational -> Text
displayPercentage = show @_ @Decimal . (*100) . realFracToDecimal 4

-------------------------------------------------
-- Display Class
-------------------------------------------------
-- | An alternative `Show` class that can be customized without breaking:
-- `show . read = read . show`
class Display a where
  display :: a -> Text

-------------------------------------------------
-- Orphans
-------------------------------------------------
instance FromFractional Decimal where
  fromFractional = realToFrac

instance ToText Decimal where
  toText = show

instance Printf.PrintfArg Decimal where
  formatArg x fmt | Printf.fmtChar (Printf.vFmt 'D' fmt) == 'D' =
    Printf.formatString (show x) (fmt { Printf.fmtChar = 's', Printf.fmtPrecision = Nothing })
  formatArg _ fmt = Printf.errorBadFormat $ Printf.fmtChar fmt

instance Aeson.FromJSON Decimal where
  parseJSON = Aeson.withScientific "Decimal" (maybe mzero return . readMaybe . show)

instance Aeson.ToJSON Decimal where
  toJSON = Aeson.toJSON @Double . Unsafe.read . show

instance FromField Time.POSIXTime where
  fromField (Field (SQLText t) _) = maybe mzero Ok $ readMaybe $ toString t
  fromField f = returnError ConversionFailed f "need a text"

instance ToField Time.POSIXTime where
  toField = toField . show @Text

instance Default.Default Time.Day where
  -- The default is the beginning of utc time.
  def = Time.localDay $ Time.utcToLocalTime Time.utc $ Time.posixSecondsToUTCTime 0
