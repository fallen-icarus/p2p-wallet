{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapCreation where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Creation
-------------------------------------------------
-- | Information for a new swap. This is used for both one-way and two-way swaps.
data SwapCreation = SwapCreation
  -- | The target bech32 address for the new swap.
  { paymentAddress :: PaymentAddress
  -- | The swap type.
  , swapType :: SwapType
  -- | The desired offer asset. The quantity is inside the `OfferAsset`.
  , offerAsset :: OfferAsset
  -- | The desired ask asset. The quantity is inside the `AskAsset`.
  , askAsset :: AskAsset
  -- | Trading pair is inverted. This is useful for editing new swaps.
  , tradingPairInverted :: Bool
  -- | The price for converting the offer asset to the ask asset (Ask/Offer).
  , askPerOfferPrice :: Rational
  -- | The price for converting the ask asset to the offer asset (Offer/Ask). This is only needed 
  -- for two-way swaps. 
  , offerPerAskPrice :: Maybe Rational
  -- | The amount the user is will to pay to have an arbitrager satisfy this swap on their behalf.
  -- In other words, it is how much the user is willing to pay for someone else to take on the
  -- concurrency risk. This is a percentage.
  , arbitrageFee :: Rational
  -- | The number of desired new swaps with these details.
  , count :: Int
  -- | The amount of ada used for the min UTxO value. This is always in addition to any ada used for
  -- the actual swap positions.
  , deposit :: Lovelace
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,SwapCreation) where
  assetBalancesForChange xs =
      -- Increase the quantity of lovelace for each output by the count. Account for ADA possibly
      -- being the offer or ask asset, and for the ada deposit required with each swap UTxO.
      ( sum $ for xs $ \(_,SwapCreation{count,offerAsset,askAsset,deposit}) -> 
          fromIntegral (negate count) * sum
            [ lovelaceQuantity $ unOfferAsset offerAsset
            , lovelaceQuantity $ unAskAsset askAsset
            , deposit
            ]
      -- Increase the quantity of each native asset by the count.
      , flip concatMap xs $ \(_,SwapCreation{count,offerAsset,askAsset}) -> catMaybes
          [ toNativeAssetQuantity (negate count) $ unOfferAsset offerAsset
          , toNativeAssetQuantity (negate count) $ unAskAsset askAsset
          ]
      )
    where
      lovelaceQuantity :: NativeAsset -> Lovelace
      lovelaceQuantity NativeAsset{policyId,quantity}
        | policyId == "" = Lovelace quantity
        | otherwise = 0

      toNativeAssetQuantity :: Int -> NativeAsset -> Maybe NativeAsset
      toNativeAssetQuantity count asset@NativeAsset{policyId}
        | policyId == "" = Nothing
        | otherwise = Just $ asset & #quantity %~ (fromIntegral count *)

-------------------------------------------------
-- New Swap Creation
-------------------------------------------------
-- | Information from the user that will be verified and converted to a `SwapCreation`.
data NewSwapCreation = NewSwapCreation
  -- | The target bech32 address for the new swap.
  { paymentAddress :: PaymentAddress
  -- | The type of the swap.
  , swapType :: SwapType
  -- | The desired offer asset.
  , offerAsset :: OfferAsset
  -- | The desired ask asset.
  , askAsset :: AskAsset
  -- | The trading pair is inverted: the offer asset is actually being sold.
  , tradingPairInverted :: Bool
  -- | The amount of the offer asset being deposited into the swap.
  , offerQuantity :: Text
  -- | The amount of the ask asset being deposited into the swap.
  , askQuantity :: Maybe Text
  -- | The price for converting the offer asset to the ask asset.
  , askPerOfferPrice :: Text
  -- | The price for converting the ask asset to the offer asset. This is only needed for
  -- two-way swaps.
  , offerPerAskPrice :: Maybe Text
  -- | The amount the user is will to pay to have an arbitrager satisfy this swap on their behalf.
  -- In other words, it is how much the user is willing to pay for someone else to take on the
  -- concurrency risk. This is only used for one-way swaps since two-way swaps are already assumed
  -- to incentive others to swap against them.
  , arbitrageFee :: Text
  -- | This is used internally to preserve the current count when converting back from SwapCreation.
  , count :: Int
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewSwapCreation

instance Default NewSwapCreation where
  def = NewSwapCreation
    { paymentAddress = ""
    , swapType = LimitOrder
    , offerAsset = def
    , askAsset = def
    , tradingPairInverted = False
    , offerQuantity = ""
    , askQuantity = Nothing
    , askPerOfferPrice = ""
    , offerPerAskPrice = Nothing
    , arbitrageFee = "0.0"
    , count = 1
    , network = def
    }

-- | Create a fresh `NewSwapCreation` for the trading pair.
createNewSwapCreation :: Network -> (OfferAsset,AskAsset) -> NewSwapCreation
createNewSwapCreation network (offerAsset,askAsset) =
  def & #offerAsset .~ offerAsset
      & #askAsset .~ askAsset
      & #network .~ network

-- | Clear the new swap creation while leaving the trading pair info.
clearNewSwapCreation :: NewSwapCreation -> NewSwapCreation
clearNewSwapCreation NewSwapCreation{offerAsset,askAsset,network,tradingPairInverted} =
  -- Just keep the fields that must stay the same.
  def & #offerAsset .~ offerAsset
      & #askAsset .~ askAsset
      & #network .~ network
      & #tradingPairInverted .~ tradingPairInverted

-------------------------------------------------
-- NewSwapCreation <--> SwapCreation
-------------------------------------------------
-- | Verify the user info for the new swap creation.
verifyNewSwapCreation 
  :: PaymentAddress 
  -> ReverseTickerMap 
  -> NewSwapCreation 
  -> Either Text SwapCreation
verifyNewSwapCreation swapAddress reverseTickerMap NewSwapCreation{..} = do
  -- Units are always in terms of the ask asset because that is what the order book always
  -- shows.

  askPerOffer <-
    if tradingPairInverted then
      -- This price is already in terms of the offer asset, which in this case, is the ask asset.
      parseFormattedPrice 
        reverseTickerMap
        (unOfferAsset offerAsset)
        (unAskAsset askAsset) 
        askPerOfferPrice 
    else
      -- This price needs to be uninverted since the smart contract requires the price to
      -- be in terms of the offer asset.
      (1/) <$> parseFormattedPrice 
        reverseTickerMap
        (unOfferAsset offerAsset)
        (unAskAsset askAsset) 
        askPerOfferPrice 

  when (askPerOffer <= 0) $ Left "All prices must be > 0."

  offerPerAsk <- flip (maybe $ Right Nothing) offerPerAskPrice $ fmap Just .
    -- This price is already in terms of the ask asset.
    parseFormattedPrice reverseTickerMap (unOfferAsset offerAsset) (unAskAsset askAsset)

  whenJust offerPerAsk $ \price ->
    when (price <= 0) $ Left "All prices must be > 0."

  -- When `tradingPairInverted` is true, then the quantity given is actually for the ask asset
  -- and the ask asset is actually the one being offered.
  updatedOfferAsset <-
    if tradingPairInverted then
      parseFormattedAssetQuantity reverseTickerMap (unAskAsset askAsset) offerQuantity
    else
      parseFormattedAssetQuantity reverseTickerMap (unOfferAsset offerAsset) offerQuantity

  when (updatedOfferAsset ^. #quantity <= 0) $
    Left "All deposit quantities must be > 0."

  updatedAskAsset <- case askQuantity of
    Nothing -> 
      -- This path is only taken by limit orders which use `tradingPairInverted`.
      if tradingPairInverted then
        Right $ unOfferAsset offerAsset & #quantity .~ 0
      else
        Right $ unAskAsset askAsset & #quantity .~ 0
    Just amount -> 
      -- `tradingPairInverted` is not used for liquidity swaps so this value is actually for
      -- the ask asset.
      parseFormattedAssetQuantity reverseTickerMap (unAskAsset askAsset) amount

  whenJust askQuantity $ \_ ->
    when (updatedAskAsset ^. #quantity <= 0) $
      Left "All deposit quantities must be > 0."

  arbitrageFee' <- 
    if arbitrageFee == "" then
      return 0
    else
      maybeToRight ("Could not parse: " <> arbitrageFee) $ parsePercentage arbitrageFee

  when (arbitrageFee' < 0) $
    Left "The arbitrage fee must be a percentage >= 0."

  return $ SwapCreation
    { paymentAddress = swapAddress
    , askPerOfferPrice = askPerOffer
    , offerPerAskPrice = offerPerAsk
    , offerAsset = 
        if tradingPairInverted 
        then OfferAsset updatedAskAsset 
        else OfferAsset updatedOfferAsset
    , askAsset =
        if tradingPairInverted 
        then AskAsset updatedOfferAsset 
        else AskAsset updatedAskAsset
    , arbitrageFee = arbitrageFee'
    , count = count
    , deposit = 0 -- This will be set later.
    , network = network
    , swapType = swapType
    -- This will be used to re-invert things for editing.
    , tradingPairInverted = tradingPairInverted
    }

-- | Convert a `SwapCreation` back to a `NewSwapCreation` for editing. This needs to invert
-- information if required.
toNewSwapCreation :: ReverseTickerMap -> SwapCreation -> NewSwapCreation
toNewSwapCreation reverseTickerMap SwapCreation{..} = NewSwapCreation
  { paymentAddress = paymentAddress
  , swapType = swapType
  , offerAsset = 
      -- Set the offer asset quantity back to zero since another field will contain it. This 
      -- depends on whether the trading pair is inverted.
      if tradingPairInverted then
        OfferAsset $ unAskAsset askAsset & #quantity .~ 0
      else
        offerAsset & #unOfferAsset % #quantity .~ 0
  , askAsset = 
      -- Set the ask asset quantity back to zero since another field will contain it. This 
      -- depends on whether the trading pair is inverted.
      if tradingPairInverted then
        AskAsset $ unOfferAsset offerAsset & #quantity .~ 0
      else
        askAsset & #unAskAsset % #quantity .~ 0
  , offerQuantity = 
      -- Format the asset quantity. This depends on whether the trading pair is inverted.
      if tradingPairInverted then
        showAssetQuantityOnly reverseTickerMap $ unAskAsset askAsset
      else
        showAssetQuantityOnly reverseTickerMap $ unOfferAsset offerAsset
  , askQuantity = 
      if tradingPairInverted then 
        if offerAsset ^. #unOfferAsset % #quantity == 0 then
          -- The `SwapCreation` can only have a quantity of 0 if the original `NewSwapCreation` had
          -- `Nothing` from being used for a limit order.
          Nothing
        else 
          Just $ showAssetQuantityOnly reverseTickerMap $ unOfferAsset offerAsset
      else 
        if askAsset ^. #unAskAsset % #quantity == 0 then
          -- The `SwapCreation` can only have a quantity of 0 if the original `NewSwapCreation` had
          -- `Nothing` from being used for a limit order.
          Nothing
        else 
          Just $ showAssetQuantityOnly reverseTickerMap $ unAskAsset askAsset
  , askPerOfferPrice = 
      if tradingPairInverted then
        askPerOfferPrice
          & showPriceFormatted reverseTickerMap (unOfferAsset offerAsset) (unAskAsset askAsset)
      else
        -- It only needs to be inverted if the trading pair is NOT inverted.
        askPerOfferPrice
          & (1 /)
          & showPriceFormatted reverseTickerMap (unOfferAsset offerAsset) (unAskAsset askAsset)
  , offerPerAskPrice = 
      if tradingPairInverted then
        offerPerAskPrice 
          <&> (1 /)
          <&> showPriceFormatted reverseTickerMap (unAskAsset askAsset) (unOfferAsset offerAsset)
      else
        offerPerAskPrice 
          <&> showPriceFormatted reverseTickerMap (unOfferAsset offerAsset) (unAskAsset askAsset)
  , arbitrageFee = displayPercentage arbitrageFee
  , count = count
  , network = network
  , tradingPairInverted = tradingPairInverted
  }
