{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Asset where

import Data.Aeson
import Prettyprinter (Pretty(..))
import Text.Printf qualified as Printf
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import Data.Decimal (decimalPlaces)
import Data.Map qualified as Map

import P2PWallet.Prelude
import P2PWallet.Plutus

-------------------------------------------------
-- Lovelace
-------------------------------------------------
-- | A type representing lovelace values.
newtype Lovelace = Lovelace { unLovelace :: Integer }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

instance Pretty Lovelace where
  pretty (Lovelace l) = pretty l

instance FromJSON Lovelace where
  -- It is usually returned from Koios as a string.
  parseJSON = withText "Lovelace" (maybe mzero (return . Lovelace) . readMaybe . toString)

instance ToJSON Lovelace where
  toJSON = toJSON . show @String . unLovelace

makeFieldLabelsNoPrefix ''Lovelace

-------------------------------------------------
-- Ada
-------------------------------------------------
-- | A type representing Ada values.
newtype Ada = Ada { unAda :: Decimal }
  deriving (Show)
  deriving newtype (Eq,Ord,Num)

instance Pretty Ada where
  pretty (Ada a) = show a

instance Printf.PrintfArg Ada where
  formatArg (Ada x) fmt | Printf.fmtChar (Printf.vFmt 'D' fmt) == 'D' =
    Printf.formatString (show x) (fmt { Printf.fmtChar = 's', Printf.fmtPrecision = Nothing })
  formatArg _ fmt = Printf.errorBadFormat $ Printf.fmtChar fmt

makeFieldLabelsNoPrefix ''Ada

-- | Read the amount of ada meant for a new UTxO. Zero is not a valid amount of ada since
-- all UTxOs must have at least some ada.
readAda :: Text -> Either Text Ada
readAda text = do
  -- It must be a number.
  decimal <- maybeToRight "Not a valid ada quantity" $ readMaybe @Decimal $ toString text

  -- The number can have no more than 6 decimal places.
  when (decimalPlaces decimal > 6) $ Left "Ada only has up to 6 decimal places."

  -- The number must be greater than 0. All UTxOs require ada.
  when (decimal <= 0) $ Left "All UTxOs require at least some ada."

  return $ Ada decimal


-------------------------------------------------
-- Ada <-> Lovelace
-------------------------------------------------
toAda :: Lovelace -> Ada
toAda (Lovelace l) = Ada $ realFracToDecimal 6 $ (/1_000_000) $ toRational l

toLovelace :: Ada -> Lovelace
toLovelace (Ada a) = Lovelace $ round $ toRational $ a * 1_000_000

-------------------------------------------------
-- Native Assets
-------------------------------------------------
-- | The type representing native assets. ADA is not considered a native
-- asset.
data NativeAsset = NativeAsset
  { policyId :: Text
  , tokenName :: Text
  , fingerprint :: Text
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
      [ "policy_id" .= policyId
      , "asset_name" .= tokenName
      , "fingerprint" .= fingerprint
      , "quantity" .= show @String quantity
      ]

instance FromJSON NativeAsset where
  parseJSON = 
    withObject "NativeAsset" $ \o ->
      NativeAsset
        <$> o .: "policy_id"
        <*> o .: "asset_name"
        <*> o .: "fingerprint"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)

readNativeAsset :: Text -> Maybe NativeAsset
readNativeAsset t = case words $ replace "." " " t of
  [policy,name] -> do
    fingerprint <- rightToMaybe $ mkAssetFingerprint policy name
    return $ NativeAsset policy name fingerprint 0
  [num,policy,name] -> do
    n <- readMaybe @Integer $ toString num
    fingerprint <- rightToMaybe $ mkAssetFingerprint policy name
    return $ NativeAsset policy name fingerprint n
  _ -> Nothing

fullName :: Getter NativeAsset Text
fullName = to fullName'
  where
    fullName' :: NativeAsset -> Text
    fullName' NativeAsset{policyId,tokenName} = policyId <> "." <> tokenName

fullNameAndQuantity :: Getter NativeAsset Text
fullNameAndQuantity = to get'
  where
    get' :: NativeAsset -> Text
    get' NativeAsset{quantity,policyId,tokenName} = 
      show quantity <> " " <> policyId <> "." <> tokenName

-- | Native assets can be one of:
-- '# policy_id.asset_name'
-- '# fingerprint'
-- '# ticker'
-- The assets must be separated by newlines and all quantities must be greater than or equal to 0.
parseNativeAsset 
  :: Map Text (Text,Text,Word8) 
  -> Map Text (Text,Text) 
  -> Text 
  -> Either Text NativeAsset
parseNativeAsset tickerMap fingerprintMap assetLine =
    case words assetLine of
      [num,name] -> do
        asset <- maybeToRight parseErrorMsg $ asum
          [ parseTickerEntry num name
          , parseOnChainEntry assetLine
          , parseFingerprintEntry num name
          ]
        if asset ^. #quantity < 0 then Left $ "Quantities must be >= 0: " <> assetLine
        else return asset
      _ -> Left $ toText parseErrorMsg
  where
    parseErrorMsg :: Text
    parseErrorMsg = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# fingerprint'"
      , "'# ticker'"
      , ""
      , "Could not parse: '" <> assetLine <> "'"
      ]

    parseTickerEntry :: Text -> Text -> Maybe NativeAsset
    parseTickerEntry num name =
      case Map.lookup name tickerMap of
        Nothing -> Nothing
        Just (policy,assetName,decimal) -> do
          rawQuantity <- readMaybe @Decimal $ toString num
          readNativeAsset $ unwords
            [ show (unFormatQuantity decimal rawQuantity)
            , policy <> "." <> assetName
            ]

    parseOnChainEntry :: Text -> Maybe NativeAsset
    parseOnChainEntry asset = readNativeAsset asset

    parseFingerprintEntry :: Text -> Text -> Maybe NativeAsset
    parseFingerprintEntry num name =
      case Map.lookup name fingerprintMap of
        Nothing -> Nothing
        Just (policy,assetName) -> do
          readNativeAsset $ unwords
            [ num
            , policy <> "." <> assetName
            ]
