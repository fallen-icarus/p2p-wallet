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
  , module P2PWallet.Data.Core.Wallets.PaymentWallet
  , module P2PWallet.Data.Core.Wallets.StakeWallet
  ) where

import P2PWallet.Data.Core.Internal.PrimaryKeys
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.Core.Wallets.PaymentWallet
import P2PWallet.Data.Core.Wallets.StakeWallet
import P2PWallet.Prelude

data Wallets = Wallets
  { paymentWallets :: [PaymentWallet]
  , stakeWallets :: [StakeWallet]
  , dexWallets :: [DexWallet]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Wallets

instance Default Wallets where
  def = Wallets
    { paymentWallets = []
    , stakeWallets = []
    , dexWallets = []
    }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update all wallets using that `stakeId` so they all use the same alias.
updateStakeIdAliases :: StakeId -> Text -> Wallets -> Wallets
updateStakeIdAliases stakeId newAlias wallets =
    wallets
      & #stakeWallets %~ update
      & #dexWallets %~ update
  where
    update 
      :: ( Default a
         , LabelOptic "stakeId" A_Lens a a StakeId StakeId
         , LabelOptic "alias" A_Lens a a Text Text
         ) 
      => [a] -> [a]
    update ws = 
      let target = fromMaybe def $ find ((stakeId ==) . view #stakeId) ws
          rest = filter ((stakeId /=) . view #stakeId) ws
          updatedTarget = target & #alias .~ newAlias
      in sortOn (view #stakeId) $ updatedTarget : rest

-- | Delete all wallets using that `stakeId`.
deleteStakeIdWallets :: StakeId -> Wallets -> Wallets
deleteStakeIdWallets stakeId wallets =
    wallets
      & #stakeWallets %~ delete
      & #dexWallets %~ delete
  where
    delete :: (Default a , LabelOptic "stakeId" A_Lens a a StakeId StakeId) => [a] -> [a]
    delete = filter ((stakeId /=) . view #stakeId)
