{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Close
-------------------------------------------------
-- | Information for closing a swap. This is used for both one-way and two-way swaps.
data SwapClose = SwapClose
  { utxoRef :: TxOutRef
  -- | The stake bech32 address for this input. This is used to get any required key hashes.
  , stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current swap terms.
  , swapDatum :: Maybe SwapDatum
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | The type of swap being closed.
  , swapType :: SwapType
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapClose

instance AssetBalancesForChange (a,SwapClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> and
        [ policyId /= OneWay.beaconCurrencySymbol
        , policyId /= TwoWay.beaconCurrencySymbol
        ]

swapUTxOToSwapClose 
  :: Network
  -> Text 
  -> StakeAddress 
  -> Maybe DerivationInfo
  -> SwapUTxO 
  -> SwapClose
swapUTxOToSwapClose network alias stakeAddress mKeyInfo SwapUTxO{..} = 
  SwapClose
    { utxoRef = utxoRef
    , swapType = swapType
    , stakeAddress = stakeAddress
    , stakeKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , swapDatum = swapDatum
    , walletAlias = alias
    , network = network
    }
