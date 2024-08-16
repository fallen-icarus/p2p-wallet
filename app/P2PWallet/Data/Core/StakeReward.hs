{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

In order to store the stake rewards in the sqlite database, the Koios version must be converted to a
new form.

-}
module P2PWallet.Data.Core.StakeReward where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.StakeReward qualified as Koios
import P2PWallet.Prelude

data StakeReward = StakeReward
  { earnedEpoch :: Integer
  , spendableEpoch :: Integer
  , amount :: Lovelace
  , poolId :: PoolID
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

makeFieldLabelsNoPrefix ''StakeReward

toStakeReward :: Koios.StakeReward -> StakeReward
toStakeReward Koios.StakeReward{..} = StakeReward
  { earnedEpoch = earnedEpoch
  , spendableEpoch = spendableEpoch
  , amount = amount
  , poolId = poolId
  }
