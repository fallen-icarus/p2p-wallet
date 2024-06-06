{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
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

import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.PoolID
import P2PWallet.Data.Core.RegistrationStatus
import P2PWallet.Prelude

data StakeAccount = StakeAccount
  { stakeAddress :: StakeAddress
  , registrationStatus :: RegistrationStatus
  , totalDelegation :: Lovelace
  , utxoBalance :: Lovelace
  , availableRewards :: Lovelace
  , delegatedPool :: Maybe PoolID
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
