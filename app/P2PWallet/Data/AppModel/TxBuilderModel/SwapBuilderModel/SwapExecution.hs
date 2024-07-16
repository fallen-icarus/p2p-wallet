{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapExecution where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Execution
-------------------------------------------------
-- | Information for executing a swap. This is used for both one-way and two-way swaps.
data SwapExecution = SwapExecution
  -- | The output reference for the swap being executed.
  { utxoRef :: TxOutRef
  -- | The swap address.
  , swapAddress :: PaymentAddress
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current swap terms.
  , swapDatum :: Maybe SwapDatum
  -- | The required swap ratio.
  , askPerOfferPrice :: Rational
  -- | The type of swap being closed.
  , swapType :: SwapType
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The swap's offer asset and how much is being returned to the swap.
  , offerAsset :: OfferAsset
  -- | The required amount of the ask asset that must be deposited into the swap.
  , askAsset :: AskAsset
  -- | The maximum amount of the offer asset available. This is useful for editing.
  , offerAvailable :: Integer
  -- | The starting balance of the ask asset. This is useful for editing.
  , startingAskQuantity :: Integer
  -- | The minUTxOValue amount of ada.
  , deposit :: Lovelace
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapExecution

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,SwapExecution) where
  assetBalancesForChange xs = sumAssetBalances
      [ ( sum $ map (view #lovelace . snd) xs
        , filterOutBeacons $ sumNativeAssets $ concatMap (view #nativeAssets . snd) xs
        )
      , -- Account for ADA possibly being the offer or ask asset, and for the ada deposit required 
        -- with each swap UTxO.
        ( sum $ for xs $ \(_,SwapExecution{offerAsset,askAsset,deposit}) -> 
            negate $ sum
              [ lovelaceQuantity $ unOfferAsset offerAsset
              , lovelaceQuantity $ unAskAsset askAsset
              , deposit
              ]
        , flip concatMap xs $ \(_,SwapExecution{offerAsset,askAsset}) -> catMaybes
            [ toNegatedNativeAssetQuantity $ unOfferAsset offerAsset
            , toNegatedNativeAssetQuantity $ unAskAsset askAsset
            ]
        )
      ]
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> and
        [ policyId /= OneWay.beaconCurrencySymbol
        , policyId /= TwoWay.beaconCurrencySymbol
        ]

      lovelaceQuantity :: NativeAsset -> Lovelace
      lovelaceQuantity NativeAsset{policyId,quantity}
        | policyId == "" = Lovelace quantity
        | otherwise = 0

      toNegatedNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
      toNegatedNativeAssetQuantity asset@NativeAsset{policyId}
        | policyId == "" = Nothing
        | otherwise = Just $ asset & #quantity %~ negate

-- | Check that the swap UTxO satisfies the minUTxO amount. Also account for whether ada is the
-- offer asset and therefore, not all of it can be taken.
updateMinUTxO :: SwapExecution -> Lovelace -> Either Text SwapExecution
updateMinUTxO swapExecution@SwapExecution{..} minValue = do
  let totalOffer = offerAsset ^. #unOfferAsset % #quantity
      totalAsk = askAsset ^. #unAskAsset % #quantity
  if offerAsset ^. #unOfferAsset % #policyId == "" then do
    -- At least the minUTxOValue amount of ada must be returned. The `deposit` field will not be
    -- used in this scenario since the `offerAsset` field will cover it.
    when (totalOffer < unLovelace minValue) $ 
      Left $ unlines
        [ "This swap requires " <> display minValue <> " to be stored with it."
        , ""
        , unwords
            [ "The actual amount of the ADA available to buy is:"
            , display $ Lovelace offerAvailable - minValue
            ]
        ]

    -- The offer quantity being returned has enough to cover the minValue.
    return swapExecution
  else if askAsset ^. #unAskAsset % #policyId == "" then do
    -- At least the minUTxOValue amount of ada must be returned. The `deposit` field will not be
    -- used in this scenario since the `askAsset` field will cover it.
    when (totalAsk < unLovelace minValue) $ do
      Left $ unlines
        [ "The new swap output does not have enough ADA for the blockchain to accept it."
        , ""
        , unwords
            [ "This swap requires " <> display minValue <> " to be stored with it."
            , "To satisfy this, the purchase cost for this swap must be at least"
            , display (minValue - lovelace) <> "."
            ]
        ]
      
    return swapExecution
  else
    return $ swapExecution & #deposit .~ minValue

-------------------------------------------------
-- New Swap Execution
-------------------------------------------------
-- | Information for executing a swap. This is used for both one-way and two-way swaps.
data NewSwapExecution = NewSwapExecution
  -- | The output reference for the swap being executed.
  { utxoRef :: TxOutRef
  -- | The swap address.
  , swapAddress :: PaymentAddress
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current swap terms.
  , swapDatum :: Maybe SwapDatum
  -- | The required swap ratio.
  , askPerOfferPrice :: Rational
  -- | The type of swap being closed.
  , swapType :: SwapType
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The offer asset that will be returned to the swap.
  , offerAsset :: OfferAsset
  -- | The ask asset that must be deposited.
  , askAsset :: AskAsset
  -- | Desired quantity of the offer asset.
  , offerQuantity :: Text
  -- | The maximum amount of the offer asset available. This is useful for editing.
  , offerAvailable :: Integer
  -- | The starting balance of the ask asset. This is useful for editing.
  , startingAskQuantity :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewSwapExecution

instance Default NewSwapExecution where
  def = NewSwapExecution
    { utxoRef = TxOutRef "" 0
    , swapAddress = ""
    , lovelace = 0
    , nativeAssets = []
    , swapDatum = Nothing
    , askPerOfferPrice = 0
    , swapType = LimitOrder
    , network = def
    , offerAsset = def
    , askAsset = def
    , offerQuantity = ""
    , offerAvailable = 0
    , startingAskQuantity = 0
    }

-- | Create a populated `NewSwapExecution` based on the current swap terms.
swapUTxOToNewSwapExecution 
  :: Network 
  -> OfferAsset 
  -> AskAsset 
  -> ReverseTickerMap 
  -> SwapUTxO 
  -> NewSwapExecution
swapUTxOToNewSwapExecution network offerAsset askAsset reverseTickerMap u@SwapUTxO{..} = NewSwapExecution
    { utxoRef = utxoRef
    , swapAddress = swapAddress
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , swapDatum = swapDatum
    , swapType = swapType
    , network = network
    , offerAsset = 
        -- The default is to take all of it.
        OfferAsset $ updatedOfferAsset & #quantity .~ 0
    , askAsset =
        -- Account for any of the ask asset being deposited in previous partial swaps.
        AskAsset updatedAskAsset
    , offerQuantity = 
        -- The default is to take all of it.
        showAssetQuantityOnly reverseTickerMap updatedOfferAsset
    , askPerOfferPrice = 
        fromMaybe 0 $ swapUTxOPrice offerAsset askAsset u
    , offerAvailable = 
        updatedOfferAsset ^. #quantity
    , startingAskQuantity = 
        updatedAskAsset ^. #quantity
    }
  where
    updatedOfferAsset :: NativeAsset
    updatedOfferAsset = targetQuantity $ unOfferAsset offerAsset

    updatedAskAsset :: NativeAsset
    updatedAskAsset = targetQuantity $ unAskAsset askAsset

    targetQuantity :: NativeAsset -> NativeAsset
    targetQuantity asset@NativeAsset{policyId=policy, tokenName=name}
      | policy == "" = asset & #quantity .~ unLovelace lovelace
      | otherwise = fromMaybe (asset & #quantity .~ 0) $
          find (\NativeAsset{policyId,tokenName} -> policy == policyId && tokenName == name) nativeAssets

-------------------------------------------------
-- NewSwapExecution <--> SwapExecution
-------------------------------------------------
verifyNewSwapExecution :: ReverseTickerMap -> NewSwapExecution -> Either Text SwapExecution
verifyNewSwapExecution reverseTickerMap NewSwapExecution{..} = do
  desiredOfferAsset@NativeAsset{quantity=desiredQuantity} <- 
    parseFormattedAssetQuantity reverseTickerMap (unOfferAsset offerAsset) offerQuantity

  when (desiredQuantity > offerAvailable) $ Left $ unlines
    [ "The maximum amount of the offer asset available is:"
    , showAssetBalance True reverseTickerMap $ 
        unOfferAsset offerAsset & #quantity .~ offerAvailable
    ]

  return $ SwapExecution
    { utxoRef = utxoRef
    , swapAddress = swapAddress
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , swapDatum = swapDatum
    , askPerOfferPrice = askPerOfferPrice
    , swapType = swapType
    , network = network
    , offerAsset = 
        OfferAsset $ desiredOfferAsset & #quantity %~ (offerAvailable -)
    , askAsset = 
        askAsset & #unAskAsset % #quantity .~
          -- Convert the desired quantity to units of the ask asset, and add it to the starting
          -- balance for the ask asset.
          (startingAskQuantity + roundUp (fromIntegral desiredQuantity * askPerOfferPrice))
    , offerAvailable = offerAvailable
    , startingAskQuantity = startingAskQuantity
    , deposit = 0 -- This will be set later.
    }

toNewSwapExecution :: ReverseTickerMap -> SwapExecution -> NewSwapExecution
toNewSwapExecution reverseTickerMap SwapExecution{..} = NewSwapExecution
  { utxoRef = utxoRef
  , swapAddress = swapAddress
  , lovelace = lovelace
  , nativeAssets = nativeAssets
  , swapDatum = swapDatum
  , askPerOfferPrice = askPerOfferPrice
  , swapType = swapType
  , network = network
  , offerQuantity = showAssetQuantityOnly reverseTickerMap $ 
      unOfferAsset offerAsset & #quantity %~ (offerAvailable -)
  , offerAsset = offerAsset & #unOfferAsset % #quantity %~ (offerAvailable -)
  , askAsset = askAsset
  , offerAvailable = offerAvailable
  , startingAskQuantity = startingAskQuantity
  }
