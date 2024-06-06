{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Wallets 
  ( 
    Wallets(..)
  , module P2PWallet.Data.Wallets.PaymentWallet
  , module P2PWallet.Data.Wallets.StakeWallet
  ) where

import P2PWallet.Data.Wallets.PaymentWallet
import P2PWallet.Data.Wallets.StakeWallet
import P2PWallet.Prelude

data Wallets = Wallets
  { paymentWallets :: [PaymentWallet]
  , stakeWallets :: [StakeWallet]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Wallets

instance Default Wallets where
  def = Wallets
    { paymentWallets = []
    , stakeWallets = []
    }
