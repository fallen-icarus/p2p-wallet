{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapUpdate where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Update
-------------------------------------------------
-- | Information for updating a swap. This is used for both one-way and two-way swaps.
data SwapUpdate = SwapUpdate
  -- | The output reference for the swap UTxO being updated.
  { utxoRef :: TxOutRef
  -- | The target bech32 address for the new swap.
  , paymentAddress :: PaymentAddress
  -- | The swap type.
  , swapType :: SwapType
  -- | The stake bech32 address for this input. This is used to get any required key hashes.
  , stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , stakeKeyPath :: Maybe DerivationPath 
  -- | The amount of ada in the starting UTxO.
  , lovelace :: Lovelace
  -- | The native assets in the starting UTxO.
  , nativeAssets :: [NativeAsset]
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Trading pair is inverted. This is useful for editing new swaps. The units used depends
  -- on this setting.
  , tradingPairInverted :: Bool
  -- | The desired offer asset. The quantity is inside the `OfferAsset`.
  , offerAsset :: OfferAsset
  -- | The desired ask asset. The quantity is inside the `AskAsset`.
  , askAsset :: AskAsset
  -- | The price for converting the offer asset to the ask asset (Ask/Offer).
  , askPerOfferPrice :: Rational
  -- | The price for converting the ask asset to the offer asset (Offer/Ask). This is only needed 
  -- for two-way swaps. 
  , offerPerAskPrice :: Maybe Rational
  -- | The amount the user is will to pay to have an arbitrager satisfy this swap on their behalf.
  -- In other words, it is how much the user is willing to pay for someone else to take on the
  -- concurrency risk. This is a percentage.
  , arbitrageFee :: Rational
  -- | The amount of ada used for the min UTxO value. This is always in addition to any ada used for
  -- the actual swap positions.
  , deposit :: Lovelace
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapUpdate

-- This instance is used for the change calculation.
instance AssetBalances (a,SwapUpdate) where
  assetBalances negateValues xs =
      ( adjust $ inputLoves - outputLoves
      , map (over #quantity adjust) $ sumNativeAssets $ inputNativeAssets <> outputNativeAssets
      )
    where
      (inputLoves,inputNativeAssets) =
        ( sum $ map (view #lovelace . snd) xs
        , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
        )

      (outputLoves,outputNativeAssets) =
        ( sum $ for xs $ \(_,SwapUpdate{offerAsset,askAsset,deposit}) -> 
            sum
              [ lovelaceQuantity $ unOfferAsset offerAsset
              , lovelaceQuantity $ unAskAsset askAsset
              , deposit
              ]
        , flip concatMap xs $ \(_,SwapUpdate{offerAsset,askAsset}) -> catMaybes
            [ toNativeAssetQuantity $ unOfferAsset offerAsset
            , toNativeAssetQuantity $ unAskAsset askAsset
            ]
        )

      adjust :: (Num b) => b -> b
      adjust = if negateValues then negate else id

      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> and
        [ policyId /= OneWay.beaconCurrencySymbol
        , policyId /= TwoWay.beaconCurrencySymbol
        ]

      lovelaceQuantity :: NativeAsset -> Lovelace
      lovelaceQuantity NativeAsset{policyId,quantity}
        | policyId == "" = Lovelace quantity
        | otherwise = 0

      toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
      toNativeAssetQuantity asset@NativeAsset{policyId}
        | policyId == "" = Nothing
        | otherwise = Just $ asset & #quantity %~ negate
