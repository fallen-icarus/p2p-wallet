{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/account_rewards API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.StakeReward where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

data StakeReward = StakeReward
  { earnedEpoch :: Integer
  , spendableEpoch :: Integer
  , amount :: Lovelace
  , poolId :: PoolID
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''StakeReward

instance FromJSON StakeReward where
  parseJSON = withObject "StakeReward" $ \o ->
      StakeReward
        <$> o .: "earned_epoch"
        <*> o .: "spendable_epoch"
        <*> o .: "amount"
        <*> o .: "pool_id"

-- | This type is used to unwrap the koios response.
newtype StakeRewards = StakeRewards { unStakeRewards :: [StakeReward] }

instance FromJSON StakeRewards where
  parseJSON = withObject "StakeRewards" $ \o ->
      StakeRewards
        <$> o .: "rewards"
