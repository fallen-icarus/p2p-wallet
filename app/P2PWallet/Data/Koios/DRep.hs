{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/drep_info API 
endpoint (the types are the same for mainnet).

-}
module P2PWallet.Data.Koios.DRep where

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS

import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Ok (Ok(Ok))

import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- DRep
-------------------------------------------------
-- | The information about the DRep.
data DRep = DRep
  { drepId :: DRepID
  , isScript :: Bool
  , registered :: Bool
  , deposit :: Maybe Lovelace
  , active :: Bool
  , expiresEpoch :: Maybe Integer
  , amount :: Lovelace
  , url :: Maybe Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''DRep

instance ToJSON DRep where
  toJSON DRep{..} =
    object [ "drep_id" .= drepId
           , "has_script" .= isScript
           , "registered" .= registered
           , "deposit" .= deposit
           , "active" .= active
           , "expires_epoch_no" .= expiresEpoch
           , "amount" .= amount
           , "meta_url" .= url
           ]

instance FromJSON DRep where
  parseJSON = withObject "DRep" $ \o ->
    DRep
      <$> o .: "drep_id"
      <*> o .: "has_script"
      <*> o .: "registered"
      <*> o .: "deposit"
      <*> o .: "active"
      <*> o .: "expires_epoch_no"
      <*> o .: "amount"
      <*> o .: "meta_url"

instance ToField DRep where
  toField = toField . encode

instance FromField DRep where
  fromField = fmap (decode @DRep) . fromField @LBS.ByteString >=> maybe mzero Ok
