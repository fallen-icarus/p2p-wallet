{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/account_rewards API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.StakeReward where

import Data.Aeson

import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.PoolID
import P2PWallet.Prelude

data StakeReward = StakeReward
  { _earnedEpoch :: Integer
  , _spendableEpoch :: Integer
  , _amount :: Lovelace
  , _poolId :: PoolID
  } deriving (Show,Eq)

instance FromJSON StakeReward where
  parseJSON = withObject "StakeReward" $ \o ->
      StakeReward
        <$> o .: "earned_epoch"
        <*> o .: "spendable_epoch"
        <*> o .: "amount"
        <*> o .: "pool_id"
