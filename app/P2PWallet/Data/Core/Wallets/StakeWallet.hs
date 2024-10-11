{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.StakeWallet where

import Database.SQLite.Simple (ToRow,FromRow)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Core.StakeReward
import P2PWallet.Database
import P2PWallet.Prelude

-- | A stake wallet is a stake address. If the stake address is a paired hardware
-- wallet, then `stakeKeyPath` will be `Just derivationPath`. Only stake wallets
-- with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other
-- addresses, like cold wallets.
data StakeWallet = StakeWallet
  { network :: Network
  , profileId :: ProfileId
  , stakeWalletId :: StakeWalletId
  , alias :: Text
  , stakeAddress :: StakeAddress
  , stakeKeyDerivation :: Maybe DerivationInfo
  , registrationStatus :: RegistrationStatus
  , totalDelegation :: Lovelace
  , utxoBalance :: Lovelace
  , availableRewards :: Lovelace
  , delegatedPool :: Maybe Pool
  , rewardHistory :: [StakeReward]
  , linkedAddresses :: [PaymentAddress]
  } deriving (Generic,ToRow,FromRow,Show,Eq)

makeFieldLabelsNoPrefix ''StakeWallet

instance Default StakeWallet where
  def = StakeWallet 
    { network = def
    , alias = "Dummy"
    , profileId = 0
    , stakeWalletId = 0
    , stakeAddress = StakeAddress "" 
    , stakeKeyDerivation = Nothing
    , registrationStatus = NotRegistered
    , totalDelegation = 0
    , utxoBalance = 0
    , availableRewards = 0
    , delegatedPool = Nothing
    , rewardHistory = []
    , linkedAddresses = []
    }

instance TableName StakeWallet where
  tableName = "stake_wallets"

instance Creatable StakeWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE IF NOT EXISTS " <> tableName @StakeWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "stake_wallet_id INTEGER PRIMARY KEY"
        , "alias TEXT NOT NULL"
        , "stake_address TEXT"
        , "stake_key_derivation TEXT"
        , "registration_status TEXT NOT NULL"
        , "total_delegation INTEGER NOT NULL"
        , "utxo_balance INTEGER NOT NULL"
        , "available_rewards INTEGER NOT NULL"
        , "delegated_pool BLOB"
        , "reward_history BLOB"
        , "linked_addresses BLOB"
        , "UNIQUE(network,profile_id,stake_wallet_id,alias)"
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
        , "stake_wallet_id"
        , "alias"
        , "stake_address"
        , "stake_key_derivation"
        , "registration_status"
        , "total_delegation"
        , "utxo_balance"
        , "available_rewards"
        , "delegated_pool"
        , "reward_history"
        , "linked_addresses"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Notify StakeWallet where
  notify oldState@StakeWallet{delegatedPool=oldPool} newState@StakeWallet{delegatedPool=newPool}
    | null msg = Nothing
    | otherwise = Just $ Notification
        { notificationType = StakeNotification
        , alias = oldState ^. #alias
        , message = mconcat $ intersperse "\n" msg
        , markedAsRead = False
        }
    where
      isDeregistered :: Bool
      isDeregistered = oldState ^. #registrationStatus == Registered 
                    && newState ^. #registrationStatus == NotRegistered
  
      isRegistered :: Bool
      isRegistered = oldState ^. #registrationStatus == NotRegistered 
                    && newState ^. #registrationStatus == Registered

      delegationChanged :: Bool
      delegationChanged = isJust oldPool && isJust newPool 
                        && oldPool ^? _Just % #poolId /= newPool ^? _Just % #poolId

      firstDelegation :: Bool
      firstDelegation = isNothing oldPool && isJust newPool

      newPoolId :: PoolID
      newPoolId = fromMaybe "" $ newPool ^? _Just % #poolId

      rewardsDiff = newState ^. #availableRewards - oldState ^. #availableRewards

      rewardMsg
        | rewardsDiff > 0 = display rewardsDiff <> " was added to rewards."
        | rewardsDiff < 0 = display (abs rewardsDiff) <> " was withdrawn from rewards."
        | otherwise = ""

      deregisteredMsg
        | isDeregistered = "Stake credential deregistered."
        | otherwise = ""

      registeredMsg
        | isRegistered && firstDelegation = 
            "Stake credential registered and delegated to " <> display newPoolId <> "."
        | isRegistered =
            "Stake credential registered."
        | otherwise = ""

      delegationMsg
        | registeredMsg /= "" = "" -- It will cover delegation as well.
        | firstDelegation = "Delegated to " <> display newPoolId <> "."
        | delegationChanged = "Delegation changed to " <> display newPoolId <> "."
        | otherwise = ""

      msg = filter (/= "")
        [ deregisteredMsg
        , registeredMsg
        , delegationMsg
        , rewardMsg
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

-- NOTE: Validation of new stake wallet information occurs in `P2PWallet.Actions.AddWallet`.
