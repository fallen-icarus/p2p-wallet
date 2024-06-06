{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
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

import P2PWallet.Prelude
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.PoolID

-- | Whether the pool is still active, retired, or in the process of retiring.
data PoolStatus
  = RegisteredPool
  | RetiringPool
  | RetiredPool
  deriving (Show,Eq)

instance ToJSON PoolStatus where
  toJSON = toJSON . showPoolStatus

instance FromJSON PoolStatus where
  parseJSON = withText "PoolStatus" (maybe mzero return . readPoolStatus)

readPoolStatus :: Text -> Maybe PoolStatus
readPoolStatus "retiring" = Just RetiringPool
readPoolStatus "registered" = Just RegisteredPool
readPoolStatus "retired" = Just RetiredPool
readPoolStatus _ = Nothing

showPoolStatus :: PoolStatus -> Text
showPoolStatus RetiringPool = "retiring"
showPoolStatus RegisteredPool = "registered"
showPoolStatus RetiredPool = "retired"

-- | The information about the pool.
data PoolInfo = PoolInfo
  { name :: Text
  , ticker :: Text
  , homepage :: Text
  , description :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PoolInfo

instance ToJSON PoolInfo where
  toJSON PoolInfo{..} =
    object [ "name" .= name
           , "ticker" .= ticker
           , "homepage" .= homepage
           , "description" .= description
           ]

instance FromJSON PoolInfo where
  parseJSON = withObject "PoolInfo" $ \o ->
    PoolInfo
      <$> o .: "name"
      <*> o .: "ticker"
      <*> o .: "homepage"
      <*> o .: "description"
      
-- A default instance is usefull for `Maybe PoolInfo`.
instance Default PoolInfo where
  def = PoolInfo
    { name = "none"
    , ticker = "none"
    , homepage = "none"
    , description = "none"
    }

-- | The stats for a specific pool.
data Pool = Pool
  { poolId :: PoolID
  , margin :: Maybe Decimal
  , fixedCost :: Maybe Lovelace
  , pledge :: Maybe Lovelace
  , info :: Maybe PoolInfo
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

makeFieldLabelsNoPrefix ''Pool

instance ToJSON Pool where
  toJSON Pool{..} =
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

instance FromJSON Pool where
  parseJSON = withObject "Pool" $ \o ->
    Pool
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
