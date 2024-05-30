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
