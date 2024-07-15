{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapUpdate where

import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapClose
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapCreation
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Update
-------------------------------------------------
-- | A swap update is just the composition of closing one swap and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data SwapUpdate = SwapUpdate
  { oldSwap :: SwapClose
  , newSwap :: SwapCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapUpdate

instance AssetBalancesForChange (a,SwapUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldSwap) xs
    , assetBalancesForChange $ map (over _2 $ view #newSwap) xs
    ]

-- | Create a populated `NewSwapCreation` based on the current swap terms.
swapUTxOToNewSwapCreation :: Network -> ReverseTickerMap -> SwapUTxO -> NewSwapCreation
swapUTxOToNewSwapCreation network reverseTickerMap u@SwapUTxO{..} = NewSwapCreation
    { paymentAddress = swapAddress
    , swapType = swapType
    , arbitrageFee = "0.0"
    , tradingPairInverted = False
    , offerAsset = OfferAsset offerAsset
    , askAsset = AskAsset askAsset
    , offerQuantity = showAssetQuantityOnly reverseTickerMap $ targetQuantity offerAsset
    , askQuantity = 
        if swapType == LimitOrder then Nothing else 
          Just $ showAssetQuantityOnly reverseTickerMap $ targetQuantity askAsset
    , count = 1
    , network = network
    , askPerOfferPrice = 
        showPriceFormatted reverseTickerMap offerAsset askAsset $
          maybe 0 (1/) $ swapUTxOPrice (OfferAsset offerAsset) (AskAsset askAsset) u
    , offerPerAskPrice =
        if swapType == LimitOrder then Nothing else
          Just $ showPriceFormatted reverseTickerMap offerAsset askAsset $
            fromMaybe 0 $ swapUTxOPrice (OfferAsset askAsset) (AskAsset offerAsset) u
    }
  where
    offerAsset = fromMaybe lovelaceAsNativeAsset 
               $ swapUTxOOfferAsset u <|> swapUTxOAsset1 u
    askAsset = fromMaybe lovelaceAsNativeAsset 
             $ swapUTxOAskAsset u <|> swapUTxOAsset2 u
    
    targetQuantity :: NativeAsset -> NativeAsset
    targetQuantity asset@NativeAsset{policyId=policy, tokenName=name}
      | policy == "" = asset & #quantity .~ unLovelace lovelace
      | otherwise = fromMaybe def $
          find (\NativeAsset{policyId,tokenName} -> policy == policyId && tokenName == name) nativeAssets
