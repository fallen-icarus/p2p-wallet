{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.OptionsKeyBurn where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Options Key Burn
-------------------------------------------------
-- | Information for burning a left over key nft.
data OptionsKeyBurn = OptionsKeyBurn
  -- | The key nft to burn.
  { contractIdAsset :: NativeAsset
  -- | Wallet burning the key.
  , walletAlias :: Text
  -- | Which network to use. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsKeyBurn

instance AssetBalancesForChange (a,OptionsKeyBurn) where
  assetBalancesForChange xs =
    ( 0
    , map (view $ _2 % #contractIdAsset) xs
    )

contractIdToOptionsKeyBurn
  :: Network
  -> Text
  -> Options.ContractId
  -> OptionsKeyBurn
contractIdToOptionsKeyBurn network alias (Options.ContractId i) = 
  OptionsKeyBurn
    { contractIdAsset = mkNativeAsset Options.activeBeaconCurrencySymbol i & #quantity .~ (-1)
    , walletAlias = alias
    , network = network
    }
