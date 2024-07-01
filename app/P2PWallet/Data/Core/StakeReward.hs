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

import Database.SQLite.Simple (ToRow(..),FromRow(..))

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.StakeReward qualified as Koios
import P2PWallet.Database
import P2PWallet.Prelude

data StakeReward = StakeReward
  { profileId :: ProfileId
  , stakeId :: StakeId
  , earnedEpoch :: Integer
  , spendableEpoch :: Integer
  , amount :: Lovelace
  , poolId :: PoolID
  } deriving (Show,Eq,Generic,ToRow,FromRow)

makeFieldLabelsNoPrefix ''StakeReward

toStakeReward :: ProfileId -> StakeId -> Koios.StakeReward -> StakeReward
toStakeReward profileId stakeId Koios.StakeReward{..} = StakeReward
  { profileId = profileId
  , stakeId = stakeId
  , earnedEpoch = earnedEpoch
  , spendableEpoch = spendableEpoch
  , amount = amount
  , poolId = poolId
  }

instance TableName StakeReward where
  tableName = "rewards"

instance Creatable StakeReward where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @StakeReward
    , "("
    , unwords $ intersperse ","
        [ "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "stake_id INTEGER REFERENCES stake_wallets (stake_id)"
        , "earned_epoch INTEGER NOT NULL"
        , "spendable_epoch INTEGER NOT NULL"
        , "amount INTEGER NOT NULL"
        , "pool_id TEXT NOT NULL"
        , "PRIMARY KEY(stake_id,earned_epoch)"
        ]
    , ");"
    ]

instance Insertable StakeReward where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @StakeReward
    , "("
    , unwords $ intersperse ","
        [ "profile_id"
        , "stake_id"
        , "earned_epoch"
        , "spendable_epoch"
        , "amount"
        , "pool_id"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?);"
    ]
