{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets 
  ( 
    Wallets(..)
  , updateStakeIdAliases
  , deleteStakeIdWallets

  , module P2PWallet.Data.Core.Wallets.DexWallet
  , module P2PWallet.Data.Core.Wallets.LoanWallet
  , module P2PWallet.Data.Core.Wallets.MarketWallet
  , module P2PWallet.Data.Core.Wallets.OptionsWallet
  , module P2PWallet.Data.Core.Wallets.PaymentWallet
  , module P2PWallet.Data.Core.Wallets.StakeWallet
  ) where

import P2PWallet.Data.Core.Internal.PrimaryKeys
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.Core.Wallets.PaymentWallet
import P2PWallet.Data.Core.Wallets.StakeWallet
import P2PWallet.Prelude

data Wallets = Wallets
  { paymentWallets :: [PaymentWallet]
  , stakeWallets :: [StakeWallet]
  , dexWallets :: [DexWallet]
  , loanWallets :: [LoanWallet]
  , optionsWallets :: [OptionsWallet]
  , marketWallets :: [MarketWallet]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Wallets

instance Default Wallets where
  def = Wallets
    { paymentWallets = []
    , stakeWallets = []
    , dexWallets = []
    , loanWallets = []
    , optionsWallets = []
    , marketWallets = []
    }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update all wallets using that `stakeId` so they all use the same alias.
updateStakeIdAliases :: StakeWalletId -> Text -> Wallets -> Wallets
updateStakeIdAliases stakeWalletId newAlias wallets =
    wallets
      & #stakeWallets %~ update
      & #dexWallets %~ update
      & #loanWallets %~ update
      & #optionsWallets %~ update
      & #marketWallets %~ update
  where
    update 
      :: ( Default a
         , LabelOptic "stakeWalletId" A_Lens a a StakeWalletId StakeWalletId
         , LabelOptic "alias" A_Lens a a Text Text
         ) 
      => [a] -> [a]
    update ws = case find ((stakeWalletId ==) . view #stakeWalletId) ws of
      -- A stake wallet may not be used for defi wallets, and therefore, won't be found in the
      -- list.
      Nothing -> ws
      Just target ->
        let rest = filter ((stakeWalletId /=) . view #stakeWalletId) ws
            updatedTarget = target & #alias .~ newAlias
        in sortOn (view #stakeWalletId) $ updatedTarget : rest

-- | Delete all wallets using that `stakeId`.
deleteStakeIdWallets :: StakeWalletId -> Wallets -> Wallets
deleteStakeIdWallets stakeWalletId wallets =
    wallets
      & #stakeWallets %~ delete
      & #dexWallets %~ delete
      & #loanWallets %~ delete
      & #optionsWallets %~ delete
      & #marketWallets %~ delete
  where
    delete 
      :: (Default a , LabelOptic "stakeWalletId" A_Lens a a StakeWalletId StakeWalletId) 
      => [a] -> [a]
    delete = filter ((stakeWalletId /=) . view #stakeWalletId)
