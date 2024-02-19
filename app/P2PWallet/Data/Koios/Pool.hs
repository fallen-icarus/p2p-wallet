{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/pool_info API 
endpoint (the types are the same for mainnet).

-}
module P2PWallet.Data.Koios.Pool where

import Data.Aeson

import P2PWallet.Prelude
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.PoolID

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

data PoolInfo = PoolInfo
  { _name :: Text
  , _ticker :: Text
  , _homepage :: Text
  , _description :: Text
  } deriving (Show,Eq)

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
    { _name = "none"
    , _ticker = "none"
    , _homepage = "none"
    , _description = "none"
    }

data Pool = Pool
  { _poolId :: PoolID
  , _margin :: Maybe Decimal
  , _fixedCost :: Maybe Lovelace
  , _pledge :: Maybe Lovelace
  , _info :: Maybe PoolInfo
  , _status :: PoolStatus
  , _retiringEpoch :: Maybe Integer
  , _activeStake :: Maybe Lovelace
  , _sigma :: Maybe Decimal
  , _blockCount :: Maybe Integer
  , _livePledge :: Maybe Lovelace
  , _liveStake :: Maybe Lovelace
  , _liveDelegators :: Maybe Integer
  , _liveSaturation :: Maybe Decimal
  } deriving (Show,Eq)

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
