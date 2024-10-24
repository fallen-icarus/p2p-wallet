{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/account_info API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.StakeAccount where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

data StakeAccount = StakeAccount
  { stakeAddress :: StakeAddress
  , registrationStatus :: RegistrationStatus
  , totalDelegation :: Lovelace
  , utxoBalance :: Lovelace
  , availableRewards :: Lovelace
  , delegatedPool :: Maybe PoolID
  , delegatedDrep :: Maybe DRepID
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''StakeAccount

instance FromJSON StakeAccount where
  parseJSON = withObject "StakeAccount" $ \o ->
      StakeAccount
        <$> o .: "stake_address"
        <*> o .: "status"
        <*> o .: "total_balance"
        <*> o .: "utxo"
        <*> o .: "rewards_available"
        <*> o .: "delegated_pool"
        <*> o .: "delegated_drep"
