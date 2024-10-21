{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoSwaps.Common where

import Data.Aeson

import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Asset
-------------------------------------------------
-- | The asset being offered.
newtype OfferAsset = OfferAsset { unOfferAsset :: NativeAsset }
 deriving (Show)
 deriving newtype (Eq,Ord,ToJSON,FromJSON,Default)

makeFieldLabelsNoPrefix ''OfferAsset

-------------------------------------------------
-- Ask Asset
-------------------------------------------------
-- | The asset being asked for.
newtype AskAsset = AskAsset { unAskAsset :: NativeAsset }
 deriving (Show)
 deriving newtype (Eq,Ord,ToJSON,FromJSON,Default)

makeFieldLabelsNoPrefix ''AskAsset

-------------------------------------------------
-- Swap Type
-------------------------------------------------
-- | Swap Type
data SwapType
  = LimitOrder
  | LiquiditySwap
  deriving (Show,Eq,Generic,FromJSON,ToJSON)

makePrisms ''SwapType
