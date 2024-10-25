{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , Time.TimeLocale(..)
  , showLocalTime
  , showLocalDate
  , Time.getCurrentTimeZone
  , getCurrentDay
  , Time.addDays
  , timeStampToFilePath
  , calcDaysInPosixPeriod
  , convertDaysToPosixPeriod

    -- * Defaults
  , Default.Default(..)

    -- * Decimal
  , Decimal
  , realFracToDecimal
  
    -- * File Paths and Directories
  , getTemporaryDirectory
  , expandFilePath
  , getDatabasePath

    -- * Miscelleneous Functions
  , showValue
  , valueAsByteString
  , maybeHead
  , maybeLast
  , maybeInit
  , groupInto
  , mkScaleFactor
  , formatQuantity
  , unFormatQuantity
  , for
  , roundUp

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

    -- * NothingLast
  , NothingLast(..)

    -- * Display Class
  , Display(..)

    -- * Other useful re-exports
  , is
  , module Relude
  , module Optics
  , Printf.printf
  ) where

import Relude hiding (uncons)
import Relude.Unsafe qualified as Unsafe
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FilePath ((</>), (<.>))
import System.FilePath qualified as Path
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Default qualified as Default
import Data.Decimal (Decimal,realFracToDecimal)
import Data.ByteString.Lazy qualified as LBS
import Text.Printf qualified as Printf
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson qualified as Aeson
import Monomer.Core.FromFractional(FromFractional(..))
import Control.Exception (throwIO,catch,handle)
import Optics
import Optics.Core.Extras
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

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs

maybeInit :: [a] -> Maybe [a]
maybeInit = viaNonEmpty init

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

-- | Rounding down will cause price ratios to fail so they must always be rounded up.
roundUp :: Rational -> Integer
roundUp rat
  | fromIntegral @Integer @Rational (round rat) < rat = 1 + round rat
  | otherwise = round rat

-------------------------------------------------
-- File Paths and Directories
-------------------------------------------------
-- | Expand a user provided file path into an absolute path. The file can contain environment
-- variables and the '~' for a shorthand of the home directory.
expandFilePath :: FilePath -> IO (Maybe FilePath)
expandFilePath path = do
    expandedPath <- Path.joinPath <$> mapM processPath (Path.splitDirectories path)
    return $ if Path.isValid expandedPath then Just expandedPath else Nothing
  where
    processPath :: FilePath -> IO FilePath
    processPath subPath = case subPath of
      -- The home shorthand.
      ['~'] -> Dir.getHomeDirectory
      -- The prefix for an environment variable on unix.
      ('$':xs) -> fromMaybe subPath <$> Env.lookupEnv xs
      -- The prefix for an environment variable on windows. It is assumed the windows environment
      -- variable is also followed by a '%'.
      ('%':xs) -> fromMaybe subPath <$> Env.lookupEnv (fromMaybe subPath $ viaNonEmpty init xs)
      -- This is not a shorthand for anything.
      _ -> return subPath
    
-- | A custom version of `Dir.getTemporaryDirectory` so that all temporary files are organized
-- within a subfolder.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
  tmpDir <- (<> "/p2p-wallet") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing True tmpDir -- Create the subfolder if it doesn't exist yet.
  return tmpDir

-- | A custom version of `Dir.getXdgDirectory` so that the database is stored in a parent
-- directory.
getDatabasePath :: IO FilePath
getDatabasePath = do
  dataDir <- Dir.getXdgDirectory Dir.XdgData "p2p-wallet"
  Dir.createDirectoryIfMissing True dataDir -- Create the subfolder if it doesn't exist yet.
  return $ dataDir </> "p2p-wallet" <.> "db"

-------------------------------------------------
-- Time
-------------------------------------------------
makeFieldLabelsNoPrefix ''Time.TimeLocale

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

timeStampToFilePath :: Time.TimeZone -> Time.UTCTime -> FilePath
timeStampToFilePath zone t = Time.formatTime Time.defaultTimeLocale formatter localTime
  where
    localTime = Time.utcToLocalTime zone t
    formatter = "%b-%d-%0Y-%H-%M-%S"

calcDaysInPosixPeriod :: Time.POSIXTime -> Integer
calcDaysInPosixPeriod = (`div` secondsPerDay) . round

convertDaysToPosixPeriod :: Integer -> Time.POSIXTime
convertDaysToPosixPeriod = fromInteger . (* secondsPerDay)

secondsPerDay :: Integer
secondsPerDay = 86_400

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
-- NothingLast
-------------------------------------------------
-- | Sometimes, when a `Maybe a` needs to be sorted, the `Nothing` should appear last while the
-- `Just a` should be sorted normally.
newtype NothingLast a = NothingLast (Maybe a)
  deriving newtype (Show,Eq)

instance (Ord a) => Ord (NothingLast a) where
  NothingLast (Just _) <= NothingLast Nothing = True
  NothingLast Nothing <= NothingLast (Just _) = False
  NothingLast x <= NothingLast y = x <= y

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
-- Lists are stored as JSON Blobs in the database.
instance (Aeson.ToJSON a) => ToField [a] where
  toField = toField . Aeson.encode

-- Lists are stored as JSON Blobs in the database.
instance (Aeson.FromJSON a) => FromField [a] where
  fromField = fmap (fromMaybe mzero . Aeson.decode @[a]) . fromField @LBS.ByteString

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
