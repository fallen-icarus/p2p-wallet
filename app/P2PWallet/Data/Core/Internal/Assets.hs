{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.Assets where

import Text.Printf qualified as Printf
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import Data.Decimal (decimalPlaces)
import Data.Aeson
import qualified Codec.Binary.Bech32 as Bech32
import Codec.Binary.Bech32.TH (humanReadablePart)
import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (Blake2b_160)
import Data.ByteArray (convert)

import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Lovelace
-------------------------------------------------
-- | A type representing lovelace values.
newtype Lovelace = Lovelace { unLovelace :: Integer }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

instance Display Lovelace where
  display = display . toAda

instance FromJSON Lovelace where
  -- It is usually returned from Koios as a string.
  parseJSON = withText "Lovelace" (maybe mzero (return . Lovelace) . readMaybe . toString)

instance ToJSON Lovelace where
  toJSON = toJSON . show @String . unLovelace

instance ToText Lovelace where
  toText = show . unLovelace

-------------------------------------------------
-- Ada
-------------------------------------------------
-- | A type representing Ada values.
newtype Ada = Ada { unAda :: Decimal }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToText)

instance Printf.PrintfArg Ada where
  formatArg (Ada x) fmt | Printf.fmtChar (Printf.vFmt 'D' fmt) == 'D' =
    Printf.formatString (show x) (fmt { Printf.fmtChar = 's', Printf.fmtPrecision = Nothing })
  formatArg _ fmt = Printf.errorBadFormat $ Printf.fmtChar fmt

instance Display Ada where
  display = fromString . printf "%D ADA"

-- | Read the amount of ada meant for a new UTxO. Zero is sometimes not a valid amount of ada since
-- all UTxOs must have at least some ada, while stake withdrawals can have a zero amount.
parseAda :: Bool -> Text -> Either Text Ada
parseAda canBeZero text = do
  -- It must be a number.
  decimal <- maybeToRight "Not a valid ada quantity" $ readMaybe @Decimal $ toString text

  -- The number can have no more than 6 decimal places.
  when (decimalPlaces decimal > 6) $ Left "Ada only has up to 6 decimal places."

  -- The number must not be negative.
  when (decimal < 0) $ Left "Ada quantity cannot be negative."

  -- The ada balance may not be zero.
  when (not canBeZero && decimal == 0) $ Left "Ada quantity cannot be zero."

  return $ Ada decimal

-------------------------------------------------
-- Ada <-> Lovelace
-------------------------------------------------
toAda :: Lovelace -> Ada
toAda (Lovelace l) = Ada $ realFracToDecimal 6 $ (/1_000_000) $ toRational l

toLovelace :: Ada -> Lovelace
toLovelace (Ada a) = Lovelace $ round $ toRational $ a * 1_000_000

-------------------------------------------------
-- Ada/Lovelace Optics
-------------------------------------------------
-- This is here due to the way template haskell creates module splices.
makeFieldLabelsNoPrefix ''Lovelace
makeFieldLabelsNoPrefix ''Ada

-------------------------------------------------
-- Asset Fingerprints
-------------------------------------------------
-- | A newtype for an asset fingerprint.
newtype Fingerprint = Fingerprint { unFingerprint :: Text }
  deriving (Show)
  deriving newtype (ToJSON,FromJSON,Eq,Ord,IsString)

instance Display Fingerprint where
  display = unFingerprint

makeFieldLabelsNoPrefix ''Fingerprint

-- | Calculate the asset fingerprint for the specified policy id and token name.
mkAssetFingerprint :: CurrencySymbol -> TokenName -> Fingerprint
mkAssetFingerprint (CurrencySymbol sym) (TokenName name) = do
    Fingerprint
      $ Bech32.encodeLenient hrp 
      $ Bech32.dataPartFromBytes 
      $ convert 
      $ hash @_ @Blake2b_160 
      $ unBuiltinByteString
      $ sym <> name
  where
    hrp = [humanReadablePart|asset|]

-------------------------------------------------
-- Asset Tickers
-------------------------------------------------
-- | A newtype for an asset ticker.
newtype Ticker = Ticker { unTicker :: Text }
  deriving (Show)
  deriving newtype (ToField,FromField,ToJSON,FromJSON,Eq,Ord,IsString)

instance Display Ticker where
  display = unTicker

-------------------------------------------------
-- Native Assets
-------------------------------------------------
-- | The type representing native assets. ADA is not considered a native
-- asset.
data NativeAsset = NativeAsset
  { policyId :: CurrencySymbol
  , tokenName :: TokenName
  , fingerprint :: Fingerprint
  , quantity :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NativeAsset

instance Default NativeAsset where
  def = NativeAsset
    { policyId = ""
    , tokenName = ""
    , fingerprint = ""
    , quantity = 0
    }

instance ToJSON NativeAsset where
  toJSON NativeAsset{..} = 
    object
      [ "policy_id" .= display policyId
      , "asset_name" .= display tokenName
      , "fingerprint" .= display fingerprint
      , "quantity" .= show @String quantity
      ]

instance FromJSON NativeAsset where
  parseJSON = 
    withObject "NativeAsset" $ \o ->
      NativeAsset
        <$> (o .: "policy_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "asset_name" >>= maybe mzero (return . TokenName) . parseHex)
        <*> o .: "fingerprint"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)

onChainName :: Getter NativeAsset Text
onChainName = to name
  where
    name :: NativeAsset -> Text
    name NativeAsset{policyId,tokenName} = display policyId <> "." <> display tokenName

-- | Parse a native asset of either the form: "policy_id.token_name" or "# policy_id.token_name".
parseNativeAsset :: Text -> Maybe NativeAsset
parseNativeAsset t = case words $ replace "." " " t of
  [policy,name] -> do
    policyId <- CurrencySymbol <$> parseHex policy
    tokenName <- TokenName <$> parseHex name
    let fingerprint = mkAssetFingerprint policyId tokenName
    return $ NativeAsset policyId tokenName fingerprint 0
  [num,policy,name] -> do
    n <- readMaybe @Integer $ toString num
    policyId <- CurrencySymbol <$> parseHex policy
    tokenName <- TokenName <$> parseHex name
    let fingerprint = mkAssetFingerprint policyId tokenName
    return $ NativeAsset policyId tokenName fingerprint n
  _ -> Nothing
