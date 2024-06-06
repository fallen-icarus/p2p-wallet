{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Wallets.StakeWallet where

import Data.Aeson

import Database.SQLite.Simple (field,ToRow(..),FromRow(..))
import Database.SQLite.Simple.ToField (toField)

import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.StakeReward
import P2PWallet.Prelude

-- | A stake wallet is a stake address. If the stake address is a paired hardware
-- wallet, then `stakeKeyPath` will be `Just derivationPath`. Only stake wallets
-- with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other
-- addresses, like cold wallets.
data StakeWallet = StakeWallet
  { network :: Network
  , profileId :: ProfileId
  , stakeId :: StakeId
  , alias :: Text
  , stakeAddress :: StakeAddress
  , stakeKeyPath :: Maybe DerivationPath
  , registrationStatus :: RegistrationStatus
  , totalDelegation :: Lovelace
  , utxoBalance :: Lovelace
  , availableRewards :: Lovelace
  , delegatedPool :: Maybe Pool
  , rewardHistory :: [StakeReward]
  , linkedAddresses :: [PaymentAddress]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''StakeWallet

instance Default StakeWallet where
  def = StakeWallet 
    { network = def
    , alias = "Dummy"
    , profileId = 0
    , stakeId = 0
    , stakeAddress = StakeAddress "" 
    , stakeKeyPath = Just $ StakeKeyPath 0 0
    , registrationStatus = NotRegistered
    , totalDelegation = 0
    , utxoBalance = 0
    , availableRewards = 0
    , delegatedPool = Nothing
    , rewardHistory = []
    , linkedAddresses = []
    }

instance Ord StakeWallet where
  p1 <= p2 = p1 ^. #stakeId <= p2 ^. #stakeId

instance FromRow StakeWallet where
  fromRow = do
    network <- field
    profileId <- field
    stakeId <- field
    alias <- field
    stakeAddress <- field
    stakeKeyPath <- field
    registrationStatus <- field
    totalDelegation <- field
    utxoBalance <- field
    availableRewards <- field
    delegatedPool <- fromMaybe mzero . decode <$> field
    linkedAddresses <- fromMaybe mzero . decode <$> field
    return $ StakeWallet
      { network = network
      , profileId = profileId
      , stakeId = stakeId
      , alias = alias
      , stakeAddress = stakeAddress
      , stakeKeyPath = stakeKeyPath
      , registrationStatus = registrationStatus
      , totalDelegation = totalDelegation
      , utxoBalance = utxoBalance
      , availableRewards = availableRewards
      , delegatedPool = delegatedPool
      , rewardHistory = []
      , linkedAddresses = linkedAddresses
      }

instance ToRow StakeWallet where
  toRow StakeWallet{..} =
    [ toField network
    , toField profileId
    , toField stakeId
    , toField alias
    , toField stakeAddress
    , toField stakeKeyPath
    , toField registrationStatus
    , toField totalDelegation
    , toField utxoBalance
    , toField availableRewards
    , toField $ encode delegatedPool
    , toField $ encode linkedAddresses
    ]

instance TableName StakeWallet where
  tableName = "stake_wallets"

instance Creatable StakeWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @StakeWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "stake_id INTEGER PRIMARY KEY"
        , "alias TEXT NOT NULL"
        , "stake_address TEXT"
        , "stake_key_path TEXT"
        , "registration_status TEXT NOT NULL"
        , "total_delegation INTEGER NOT NULL"
        , "utxo_balance INTEGER NOT NULL"
        , "available_rewards INTEGER NOT NULL"
        , "delegate_pool BLOB"
        , "linked_addresses BLOB"
        , "UNIQUE(network,profile_id,stake_id,alias)"
        ]
    , ");"
    ]

instance Insertable StakeWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @StakeWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "stake_id"
        , "alias"
        , "stake_address"
        , "stake_key_path"
        , "registration_status"
        , "total_delegation"
        , "utxo_balance"
        , "available_rewards"
        , "delegate_pool"
        , "linked_addresses"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

-------------------------------------------------
-- New Stake Wallet
-------------------------------------------------
-- | The type representing information the user must supply in order to track a new `PaymentWallet`.
-- There is no need to ask for a staking address when adding a watched payment wallet since it
-- can be derived from the payment address.
data NewStakeWallet = NewStakeWallet
  -- | A user-friendly name for the address. This is used regardless of pairing/watching.
  { alias :: Text 
  -- | What address index for the stake key's derivation path. The account index is set by the
  -- profile. This is only used when pairing a stake wallet.
  , stakeAddressIndex :: Int 
  -- | The new stake address to watch. This is only used when adding a watched stake wallet.
  , stakeAddress :: Text 
  -- | Whether the new wallet is being paired.
  , pairing :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewStakeWallet

instance Default NewStakeWallet where
  def = NewStakeWallet 
    { alias = ""
    , stakeAddressIndex = 0
    , stakeAddress = ""
    , pairing = True
    }
