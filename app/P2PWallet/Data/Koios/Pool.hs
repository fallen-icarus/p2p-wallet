{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/pool_info API 
endpoint (the types are the same for mainnet).

-}
module P2PWallet.Data.Koios.Pool where

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS

import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Ok (Ok(Ok))

import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Pool Status
-------------------------------------------------
-- | Whether the pool is still active, retired, or in the process of retiring.
data PoolStatus
  = RegisteredPool
  | RetiringPool
  | RetiredPool
  deriving (Show,Eq)

instance ToJSON PoolStatus where
  toJSON = toJSON . display

instance FromJSON PoolStatus where
  parseJSON = withText "PoolStatus" (maybe mzero return . parsePoolStatus)

instance Display PoolStatus where
  display RetiringPool = "retiring"
  display RegisteredPool = "registered"
  display RetiredPool = "retired"

parsePoolStatus :: Text -> Maybe PoolStatus
parsePoolStatus "retiring" = Just RetiringPool
parsePoolStatus "registered" = Just RegisteredPool
parsePoolStatus "retired" = Just RetiredPool
parsePoolStatus _ = Nothing

-------------------------------------------------
-- PoolList
-------------------------------------------------
-- | The stats for a specific pool from the pool_list endpoint. This is used for the pool picker.
data PoolList = PoolList
  { poolId :: PoolID
  , margin :: Maybe Decimal
  , fixedCost :: Maybe Lovelace
  , pledge :: Maybe Lovelace
  , ticker :: Maybe Text
  , url :: Maybe Text
  , status :: PoolStatus
  , retiringEpoch :: Maybe Integer
  , activeStake :: Maybe Lovelace
  , activeSaturation :: Maybe Decimal
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PoolList

instance ToJSON PoolList where
  toJSON PoolList{..} =
    object [ "pool_id_bech32" .= poolId
           , "margin" .= margin
           , "fixed_cost" .= fixedCost
           , "pledge" .= pledge
           , "ticker" .= ticker
           , "meta_url" .= url
           , "pool_status" .= status
           , "retiring_epoch" .= retiringEpoch
           , "active_stake" .= activeStake
           , "active_saturation" .= (Nothing :: Maybe Decimal)
           ]

instance FromJSON PoolList where
  parseJSON = withObject "PoolList" $ \o ->
    PoolList
      <$> o .: "pool_id_bech32"
      <*> o .: "margin"
      <*> o .: "fixed_cost"
      <*> o .: "pledge"
      <*> o .: "ticker"
      <*> o .: "meta_url"
      <*> o .: "pool_status"
      <*> o .: "retiring_epoch"
      <*> o .: "active_stake"
      <*> pure Nothing -- This must be calculated independentally.

instance ToField PoolList where
  toField = toField . encode

instance FromField PoolList where
  fromField = fmap (decode @PoolList) . fromField @LBS.ByteString >=> maybe mzero Ok

-------------------------------------------------
-- Pool Metadata
-------------------------------------------------
-- | The metadata for the pool.
data PoolMetadata = PoolMetadata
  { name :: Text
  , ticker :: Text
  , homepage :: Text
  , description :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PoolMetadata

instance ToJSON PoolMetadata where
  toJSON PoolMetadata{..} =
    object [ "name" .= name
           , "ticker" .= ticker
           , "homepage" .= homepage
           , "description" .= description
           ]

instance FromJSON PoolMetadata where
  parseJSON = withObject "PoolMetadata" $ \o ->
    PoolMetadata
      <$> o .: "name"
      <*> o .: "ticker"
      <*> o .: "homepage"
      <*> o .: "description"
      
-- A default instance is usefull for `Maybe PoolMetadata`.
instance Default PoolMetadata where
  def = PoolMetadata
    { name = "none"
    , ticker = "none"
    , homepage = "none"
    , description = "none"
    }

-------------------------------------------------
-- PoolInfo
-------------------------------------------------
-- | The stats for a specific pool from the pool_info endpoint.
data PoolInfo = PoolInfo
  { poolId :: PoolID
  , margin :: Maybe Decimal
  , fixedCost :: Maybe Lovelace
  , pledge :: Maybe Lovelace
  , info :: Maybe PoolMetadata
  , status :: PoolStatus
  , retiringEpoch :: Maybe Integer
  , activeStake :: Maybe Lovelace
  , sigma :: Maybe Decimal
  , blockCount :: Maybe Integer
  , livePledge :: Maybe Lovelace
  , liveStake :: Maybe Lovelace
  , liveDelegators :: Maybe Integer
  , liveSaturation :: Maybe Decimal
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PoolInfo

instance ToJSON PoolInfo where
  toJSON PoolInfo{..} =
    object [ "pool_id_bech32" .= poolId
           , "margin" .= margin
           , "fixed_cost" .= fixedCost
           , "pledge" .= pledge
           , "meta_json" .= info
           , "pool_status" .= status
           , "retiring_epoch" .= retiringEpoch
           , "active_stake" .= activeStake
           , "sigma" .= sigma
           , "block_count" .= blockCount
           , "live_pledge" .= livePledge
           , "live_stake" .= liveStake
           , "live_delegators" .= liveDelegators
           , "live_saturation" .= liveSaturation
           ]

instance FromJSON PoolInfo where
  parseJSON = withObject "PoolInfo" $ \o ->
    PoolInfo
      <$> o .: "pool_id_bech32"
      <*> o .: "margin"
      <*> o .: "fixed_cost"
      <*> o .: "pledge"
      <*> o .: "meta_json"
      <*> o .: "pool_status"
      <*> o .: "retiring_epoch"
      <*> o .: "active_stake"
      <*> o .: "sigma"
      <*> o .: "block_count"
      <*> o .: "live_pledge"
      <*> o .: "live_stake"
      <*> o .: "live_delegators"
      <*> o .: "live_saturation"

instance ToField PoolInfo where
  toField = toField . encode

instance FromField PoolInfo where
  fromField = fmap (decode @PoolInfo) . fromField @LBS.ByteString >=> maybe mzero Ok
