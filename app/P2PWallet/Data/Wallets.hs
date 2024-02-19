{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Wallets 
  ( 
    PaymentWallet(..)
  , StakeWallet (..)
  , Wallets(..)
  ) where

import Data.Aeson

import P2PWallet.Data.Wallets.PaymentWallet
import P2PWallet.Data.Wallets.StakeWallet
import P2PWallet.Prelude

-- | All wallets tracked by the app.
data Wallets = Wallets
  { _paymentWallets :: [PaymentWallet] -- ^ All payment wallets paired with the app.
  , _stakeWallets :: [StakeWallet] -- ^ All stake wallets paired with the app.
  } deriving (Show,Eq)

instance Default Wallets where
  def = Wallets
    { _paymentWallets = []
    , _stakeWallets = []
    }

instance ToJSON Wallets where
  toJSON Wallets{..} =
    object 
      [ "payment_wallets" .= _paymentWallets
      , "stake_wallets" .= _stakeWallets
      ]

instance FromJSON Wallets where
  parseJSON =
    withObject "Wallets" $ \o ->
      Wallets
        <$> o .: "payment_wallets"
        <*> o .: "stake_wallets"
