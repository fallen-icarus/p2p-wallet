{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Asset where

import Data.Aeson
import Prettyprinter (hsep,tupled,Pretty(..))
import Text.Printf qualified as Printf
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import Data.Decimal (decimalPlaces)

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
  decimal <- maybeToRight "Not a valid number" $ readMaybe @Decimal $ toString text

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

instance Pretty NativeAsset where
  pretty NativeAsset{..} =
    hsep 
      [ pretty quantity
      , pretty (policyId <> "." <> tokenName) 
      , "  " 
      , tupled [pretty fingerprint]
      ]

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
    void $ readHex policy
    void $ readHex name
    return $ NativeAsset policy name "" 0
  [num,policy,name] -> do
    n <- readMaybe @Integer $ toString num
    void $ readHex policy
    void $ readHex name
    return $ NativeAsset policy name "" n
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

-- | Native assets are supposed to be of the form '# policy_id.asset_name' and separated
-- by newlines. All quantities must be greater than or equal to 0. This is meant to be used
-- on one line at a time like: `sequence . map parseNativeAsset . lines`.
parseNativeAsset :: Text -> Either Text NativeAsset
parseNativeAsset assetLine = do
  asset@NativeAsset{quantity} <- flip maybeToRight (readNativeAsset assetLine) $
    unlines
      [ "Invalid native asset entry. Must be of the form '# policy_id.asset_name'."
      , "Native assets must be separated by newlines."
      , ""
      , "Could not parse: '" <> assetLine <> "'"
      ]

  if quantity >= 0 then Right asset else 
    Left $
      unlines 
        [ "Native asset quantities must be greater than or equal to 0."
        , ""
        , "Invalid quantity: '" <> assetLine <> "'"
        ]
