{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.Asset where

import Data.Aeson
import Prettyprinter (hsep,tupled,Pretty(..))
import Text.Printf qualified as Printf

import P2PWallet.Prelude
import P2PWallet.Data.Plutus

-------------------------------------------------
-- Lovelace
-------------------------------------------------
-- | A type representing lovelace values.
newtype Lovelace = Lovelace Integer
  deriving (Show,Eq,Num,Ord)

instance Pretty Lovelace where
  pretty (Lovelace l) = pretty l

instance FromJSON Lovelace where
  -- It is usually returned from Koios as a string.
  parseJSON = withText "Lovelace" (maybe mzero (return . Lovelace) . readMaybe . toString)

unLovelace :: Lovelace -> Integer
unLovelace (Lovelace i) = i

-------------------------------------------------
-- ADA
-------------------------------------------------
-- | A type representing ADA values.
newtype ADA = ADA Decimal
  deriving (Show,Eq,Num,Ord)

instance Pretty ADA where
  pretty (ADA a) = show a

instance Printf.PrintfArg ADA where
  formatArg (ADA x) fmt | Printf.fmtChar (Printf.vFmt 'D' fmt) == 'D' =
    Printf.formatString (show x) (fmt { Printf.fmtChar = 's', Printf.fmtPrecision = Nothing })
  formatArg _ fmt = Printf.errorBadFormat $ Printf.fmtChar fmt

unADA :: ADA -> Decimal
unADA (ADA a) = a

-------------------------------------------------
-- ADA <-> Lovelace
-------------------------------------------------
toADA :: Lovelace -> ADA
toADA (Lovelace l) = ADA $ realFracToDecimal 6 $ (/1_000_000) $ toRational l

toLovelace :: ADA -> Lovelace
toLovelace (ADA a) = Lovelace $ round $ toRational $ a * 1_000_000

-------------------------------------------------
-- Native Assets
-------------------------------------------------
-- | The type representing native assets. ADA is not considered a native
-- asset.
data NativeAsset = NativeAsset
  { _policyId :: Text
  , _tokenName :: Text
  , _fingerprint :: Text
  , _quantity :: Integer
  } deriving (Show,Eq)

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

instance Pretty NativeAsset where
  pretty NativeAsset{..} =
    hsep 
      [ pretty _quantity
      , pretty (_policyId <> "." <> _tokenName) 
      , "  " 
      , tupled [pretty _fingerprint]
      ]

instance ToJSON NativeAsset where
  toJSON NativeAsset{..} = 
    object
      [ "policy_id" .= _policyId
      , "token_name" .= _tokenName
      , "fingerprint" .= _fingerprint
      , "quantity" .= _quantity
      ]

instance FromJSON NativeAsset where
  parseJSON = 
    withObject "NativeAsset" $ \o ->
      NativeAsset
        <$> o .: "policy_id"
        <*> o .: "asset_name"
        <*> o .: "fingerprint"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)
