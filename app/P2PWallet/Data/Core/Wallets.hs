{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets 
  ( 
    Wallets(..)

  , module P2PWallet.Data.Core.Wallets.PaymentWallet
  , module P2PWallet.Data.Core.Wallets.StakeWallet
  ) where

import P2PWallet.Data.Core.Wallets.PaymentWallet
import P2PWallet.Data.Core.Wallets.StakeWallet
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
