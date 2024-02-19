{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Wallets.StakeWallet where

import Data.Aeson

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Koios.StakeReward
import P2PWallet.Prelude

-- | A stake wallet is a stake address. If the stake address is a paired hardware
-- wallet, then `_stakeKeyPath` will be `Just derivationPath`. Only stake wallets
-- with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other
-- addresses, like cold wallets.
data StakeWallet = StakeWallet
  { _alias :: Text
  , _stakeAddress :: StakeAddress
  , _stakeKeyPath :: Maybe DerivationPath
  , _registrationStatus :: RegistrationStatus
  , _totalDelegation :: Lovelace
  , _utxoBalance :: Lovelace
  , _availableRewards :: Lovelace
  , _delegatedPool :: Maybe Pool
  , _rewardHistory :: [StakeReward]
  } deriving (Show,Eq)

-- Not all of the information needs to be backed up.
instance ToJSON StakeWallet where
  toJSON StakeWallet{..} =
    object 
      [ "alias" .= _alias
      , "stake_address" .= _stakeAddress
      , "stake_key_derivation_path" .= _stakeKeyPath
      ]

instance FromJSON StakeWallet where
  parseJSON = withObject "StakeWallet" $ \o ->
      StakeWallet
        <$> o .: "alias"
        <*> o .: "stake_address"
        <*> o .: "stake_key_derivation_path"
        <*> return NotRegistered
        <*> return 0
        <*> return 0
        <*> return 0
        <*> return Nothing
        <*> return []

instance Default StakeWallet where
  def = StakeWallet 
    { _alias = "Dummy"
    , _stakeAddress = StakeAddress "" 
    , _stakeKeyPath = Just $ StakeKeyPath 0
    , _registrationStatus = NotRegistered
    , _totalDelegation = 0
    , _utxoBalance = 0
    , _availableRewards = 0
    , _delegatedPool = Nothing
    , _rewardHistory = []
    }
