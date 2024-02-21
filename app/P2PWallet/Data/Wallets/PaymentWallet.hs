{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Wallets.PaymentWallet
  ( 
    PaymentWallet(..)
  ) where

import Data.Aeson

import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Prelude

-- | A payment wallet is a payment address. If the payment address is a paired hardware
-- wallet, then `_paymentKeyPath` will be `Just derivationPath`. Only payment wallets
-- with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other
-- addresses, like cold wallets.
data PaymentWallet = PaymentWallet
  { _alias :: Text
  , _paymentAddress :: PaymentAddress
  , _paymentKeyPath :: Maybe DerivationPath
  , _stakeAddress :: Maybe StakeAddress
  , _stakeKeyPath :: Maybe DerivationPath
  , _utxos :: [AddressUTxO]
  , _txHistory :: [Transaction]
  , _nativeAssets :: [NativeAsset]
  , _lovelaces :: Lovelace
  } deriving (Show,Eq)

instance ToJSON PaymentWallet where
  toJSON PaymentWallet{..} =
    object 
      [ "alias" .= _alias
      , "payment_address" .= _paymentAddress
      , "stake_address" .= _stakeAddress
      , "payment_key_derivation_path" .= _paymentKeyPath
      , "stake_key_derivation_path" .= _stakeKeyPath
      ]

instance FromJSON PaymentWallet where
  parseJSON =
    withObject "PaymentWallet" $ \o ->
      PaymentWallet
        <$> o .: "alias"
        <*> o .: "payment_address"
        <*> o .: "payment_key_derivation_path"
        <*> o .: "stake_address"
        <*> o .: "stake_key_derivation_path"
        <*> return []
        <*> return []
        <*> return []
        <*> return 0

instance Default PaymentWallet where
  def = PaymentWallet 
    { _alias = "dummy" 
    , _paymentAddress = PaymentAddress "" 
    , _stakeAddress = Nothing 
    , _paymentKeyPath = Just $ PaymentKeyPath 0 0
    , _stakeKeyPath = Nothing 
    , _utxos = [] 
    , _txHistory = [] 
    , _nativeAssets = [] 
    , _lovelaces = 0
    }
