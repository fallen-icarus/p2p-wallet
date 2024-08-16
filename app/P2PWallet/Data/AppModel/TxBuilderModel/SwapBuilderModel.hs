{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel
  ( SwapBuilderModel(..)
  , emptySwapBuilderModel
  , isEmptySwapBuilderModel

  , SwapBuilderEvent(..)

  , module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapExecution
  , module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapUpdate
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapClose
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapCreation
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapExecution
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel.SwapUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Builder Events
-------------------------------------------------
-- | All events dealing with swaps. This is separate to help stay organized.
data SwapBuilderEvent
  -- | Remove the selected swap creation from the builder.
  = RemoveSelectedSwapCreation Int
  -- | Edit selected swap creation.
  | EditSelectedSwapCreation (AddEvent (Int,SwapCreation) (Int,SwapCreation))
  -- | Change the desired number of swap creations with these conditions. The first int is the index
  -- into the swap creations list and the second is the new count.
  | ChangeSwapCreationCount Int Int
  -- | Remove the selected swap close from the builder.
  | RemoveSelectedSwapClose Int
  -- | Remove the selected swap update from the builder.
  | RemoveSelectedSwapUpdate Int
  -- | Edit selected swap update.
  | EditSelectedSwapUpdate (AddEvent (Int,SwapUpdate) (Int,SwapCreation))
  -- | Remove the selected swap execution from the builder.
  | RemoveSelectedSwapExecution Int
  -- | Edit selected swap execution.
  | EditSelectedSwapExecution (AddEvent (Int,SwapExecution) (Int,SwapExecution))
  deriving (Show,Eq)

-------------------------------------------------
-- Swap Builder Model
-------------------------------------------------
-- | Due to the number of possible swap actions, they are grouped in this sub-model to
-- help the code stay organized.
data SwapBuilderModel = SwapBuilderModel
  { swapCreations :: [(Int,SwapCreation)]
  , targetSwapCreation :: Maybe (Int,NewSwapCreation)
  , swapCloses :: [(Int,SwapClose)]
  , swapUpdates :: [(Int,SwapUpdate)]
  -- | The `Int` is the index into `swapUpdates`.
  , targetSwapUpdate :: Maybe (Int,NewSwapCreation)
  , swapExecutions :: [(Int,SwapExecution)]
  , targetSwapExecution :: Maybe (Int,NewSwapExecution)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapBuilderModel

instance Default SwapBuilderModel where
  def = SwapBuilderModel
    { swapCreations = []
    , targetSwapCreation = Nothing
    , swapCloses = []
    , swapUpdates = []
    , targetSwapUpdate = Nothing
    , swapExecutions = []
    , targetSwapExecution = Nothing
    }

-- | This is just an alias for `def`. The name is more clear what it is.
emptySwapBuilderModel :: SwapBuilderModel
emptySwapBuilderModel = def

isEmptySwapBuilderModel :: SwapBuilderModel -> Bool
isEmptySwapBuilderModel SwapBuilderModel{..} = and
  [ null swapCreations
  , null swapCloses
  , null swapUpdates
  , null swapExecutions
  , isNothing targetSwapCreation
  , isNothing targetSwapUpdate
  , isNothing targetSwapExecution
  ]

-------------------------------------------------
-- SwapBuilderModel --> TxBody
-------------------------------------------------
-- Converting the `SwapBuilderModel` to a `TxBody`. This must be done at this level since certain
-- redeemers depend on what other swap actions are happening in the transaction.
instance AddToTxBody SwapBuilderModel where
  addToTxBody txBody SwapBuilderModel{..} = 
      txBody
        -- Add the swap creations.
        & flip (foldl' addSwapCreationToBuilder) swapCreations
        -- Add the swap closes.
        & flip (foldl' addSwapCloseToBuilder) swapCloses
        -- Add the swap updates.
        & flip (foldl' addSwapUpdateToBuilder) swapUpdates
        -- Merge any beacon mints so that there is only one `TxBodyMint` per minting policy.
        -- Remove any mints that cancel out.
        & #mints %~ mergeTxBodyMints
        & #mints %~ removeEmptyMints
        -- Adjust any redeemers based on whether all mints canceled out.
        & adjustSpendingRedeemers
        -- Add the swap executions. This is done after so that it does not mess with the beacon
        -- adjustments.
        & flip (foldl' addSwapExecutionToBuilder) swapExecutions

addSwapUpdateToBuilder :: TxBody -> (Int,SwapUpdate) -> TxBody
addSwapUpdateToBuilder txBody (idx,SwapUpdate{..}) =
  txBody
    & flip addSwapCreationToBuilder (idx,newSwap)
    & flip addSwapCloseToBuilder (idx,oldSwap)

addSwapCreationToBuilder :: TxBody -> (Int,SwapCreation) -> TxBody
addSwapCreationToBuilder txBody (_,SwapCreation{..}) =
    txBody 
      -- Add one instance of the output per count and preserve ordering of the
      -- output list.
      & #outputs %~ (<> replicate count newOutput)
      -- Add the new beacons to be minted. 
      & #mints %~ (newMint:)
  where
    lovelaceQuantity :: NativeAsset -> Lovelace
    lovelaceQuantity NativeAsset{policyId,quantity}
      | policyId == "" = Lovelace quantity
      | otherwise = 0

    toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
    toNativeAssetQuantity asset@NativeAsset{policyId}
      | policyId == "" = Nothing
      | otherwise = Just asset

    -- Get the total amount assets required for the output. This does not have the beacons
    -- yet.
    (lovelace, nativeAssets) = 
      let (OfferAsset offerAsset', AskAsset askAsset') = (offerAsset, askAsset) in
      ( deposit + lovelaceQuantity offerAsset' + lovelaceQuantity askAsset'
      , catMaybes [toNativeAssetQuantity offerAsset', toNativeAssetQuantity askAsset']
      )

    -- Adjust the swap price to reflect the arbitrage fee.
    adjustPrice :: Rational -> Rational -> Rational
    adjustPrice fee originalPrice = originalPrice * (1 - fee)

    (beaconRedeemer :: Redeemer, beaconReference :: TxOutRef) = case (network,swapType) of
      (_, LimitOrder) -> (toRedeemer OneWay.CreateOrCloseSwaps, OneWay.beaconScriptTestnetRef)
      (_, LiquiditySwap) -> (toRedeemer TwoWay.CreateOrCloseSwaps, TwoWay.beaconScriptTestnetRef)

    (swapDatum :: Datum , beacons :: [NativeAsset] , beaconSym :: CurrencySymbol) = 
      case swapType of
        LimitOrder -> 
          -- This is a one-way swap.
          let adjustedPrice = adjustPrice arbitrageFee askPerOfferPrice
              newDatum@OneWay.SwapDatum{..}
                | tradingPairInverted = 
                    -- The price should be correct but the assets themselves are flipped.
                    OneWay.genSwapDatum 
                      (OfferAsset $ unAskAsset askAsset) 
                      (AskAsset $ unOfferAsset offerAsset) 
                      adjustedPrice 
                      Nothing
                | otherwise = 
                    -- Everything is already correct.
                    OneWay.genSwapDatum offerAsset askAsset adjustedPrice Nothing
              newBeacons =
                -- One trading pair beacon.
                [ NativeAsset beaconId pairBeacon "" 1
                -- One offer beacon.
                , NativeAsset beaconId offerBeacon "" 1
                -- One Ask beacon.
                , NativeAsset beaconId askBeacon "" 1
                ]
          in (toDatum newDatum, newBeacons, beaconId)

        LiquiditySwap -> 
          -- This is a two-way swap.
          let adjustedOfferPrice = adjustPrice arbitrageFee askPerOfferPrice
              adjustedAskPrice = maybe 0 (adjustPrice arbitrageFee) offerPerAskPrice
              newDatum@TwoWay.SwapDatum{..} = 
                TwoWay.genSwapDatum 
                  (unOfferAsset offerAsset, adjustedOfferPrice)
                  (unAskAsset askAsset, adjustedAskPrice)
                  Nothing
              newBeacons =
                -- One trading pair beacon.
                [ NativeAsset beaconId pairBeacon "" 1
                -- One asset1 beacon.
                , NativeAsset beaconId asset1Beacon "" 1
                -- One asset2 beacon.
                , NativeAsset beaconId asset2Beacon "" 1
                ]
          in (toDatum newDatum, newBeacons, beaconId)

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = paymentAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets <> beacons -- Add the beacons to the output.
      , datum = OutputDatum swapDatum
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash beaconSym
      , nativeAssets = 
          -- Increase the quantity by the total number of required outputs.
          sumNativeAssets $ concat $ replicate count beacons
      , redeemer = beaconRedeemer
      , scriptWitness = ReferenceWitness beaconReference
      , executionBudget = def -- These must be calculated during the build step.
      }

addSwapCloseToBuilder :: TxBody -> (Int,SwapClose) -> TxBody
addSwapCloseToBuilder txBody (_,SwapClose{..}) =
    txBody
      -- Add the new swap to the inputs.
      & #inputs %~ flip snoc newInput
      -- Add the new beacons to be burned. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
  where
    (spendingRedeemer :: Redeemer,spendingReference :: TxOutRef,spendingScriptHash :: ScriptHash) = 
      case (network,swapType) of
        (_, LimitOrder) -> 
          ( toRedeemer OneWay.SpendWithMint
          , OneWay.swapScriptTestnetRef
          , OneWay.swapScriptHash
          )
        (_, LiquiditySwap) -> 
          ( toRedeemer TwoWay.SpendWithMint
          , TwoWay.swapScriptTestnetRef
          , TwoWay.swapScriptHash
          )

    (beaconRedeemer :: Redeemer, beaconReference :: TxOutRef) = 
      case (network,swapType) of
        (_, LimitOrder) -> (toRedeemer OneWay.CreateOrCloseSwaps, OneWay.beaconScriptTestnetRef)
        (_, LiquiditySwap) -> (toRedeemer TwoWay.CreateOrCloseSwaps, TwoWay.beaconScriptTestnetRef)

    (beacons :: [NativeAsset], beaconSym :: CurrencySymbol) = 
      case swapDatum of
        Just (OneWayDatum OneWay.SwapDatum{..}) -> 
            -- This is a one-way swap.
            let bs =
                  -- One trading pair beacon.
                  [ NativeAsset beaconId pairBeacon "" (-1)
                  -- One offer beacon.
                  , NativeAsset beaconId offerBeacon "" (-1)
                  -- One Ask beacon.
                  , NativeAsset beaconId askBeacon "" (-1)
                  ]
            in (bs, beaconId)

        Just (TwoWayDatum TwoWay.SwapDatum{..}) -> 
            -- This is a two-way swap.
            let bs =
                  -- One trading pair beacon.
                  [ NativeAsset beaconId pairBeacon "" (-1)
                  -- One asset1 beacon.
                  , NativeAsset beaconId asset1Beacon "" (-1)
                  -- One asset2 beacon.
                  , NativeAsset beaconId asset2Beacon "" (-1)
                  ]
            in (bs, beaconId)

        _ -> error $ unlines -- This path should never be taken.
          [ "SwapClose used on invalid swap UTxO:" 
          , display utxoRef
          , show swapDatum
          ]

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = spendingRedeemer
          , scriptWitness = ReferenceWitness spendingReference
          , executionBudget = def
          , scriptHash = spendingScriptHash
          }
      }

    stakeCredential :: Credential
    stakeCredential =
      -- This should have been verified at an earlier step.
      fromRight (PubKeyCredential "") $ stakeAddressToPlutusCredential stakeAddress

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash beaconSym
      , nativeAssets = beacons
      , redeemer = beaconRedeemer
      , scriptWitness = ReferenceWitness beaconReference
      , executionBudget = def -- These must be calculated during the build step.
      }

addSwapExecutionToBuilder :: TxBody -> (Int,SwapExecution) -> TxBody
addSwapExecutionToBuilder txBody (_,SwapExecution{..}) =
    txBody
      -- Add the swap to be executed to the inputs.
      & #inputs %~ flip snoc newInput
      -- Add the required swap output to the list of outputs. Preserve ordering of the output list.
      & #outputs %~ flip snoc newOutput
  where
    (spendingRedeemer :: Redeemer,spendingReference :: TxOutRef,spendingScriptHash :: ScriptHash) = 
      case (network,swapType) of
        (_, LimitOrder) -> 
          ( toRedeemer OneWay.Swap
          , OneWay.swapScriptTestnetRef
          , OneWay.swapScriptHash
          )
        (_, LiquiditySwap) -> 
          ( toRedeemer $ TwoWay.getRequiredSwapDirection offerAsset askAsset
          , TwoWay.swapScriptTestnetRef
          , TwoWay.swapScriptHash
          )

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = spendingRedeemer
          , scriptWitness = ReferenceWitness spendingReference
          , executionBudget = def
          , scriptHash = spendingScriptHash
          }
      }

    lovelaceQuantity :: NativeAsset -> Lovelace
    lovelaceQuantity NativeAsset{policyId,quantity}
      | policyId == "" = Lovelace quantity
      | otherwise = 0

    toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
    toNativeAssetQuantity asset@NativeAsset{policyId}
      | policyId == "" = Nothing
      | otherwise = Just asset

    -- Get the total amount assets required for the output. This does not have the beacons
    -- yet.
    (newLovelace, newNativeAssets) = 
      let (OfferAsset offerAsset', AskAsset askAsset') = (offerAsset, askAsset) in
      ( deposit + lovelaceQuantity offerAsset' + lovelaceQuantity askAsset'
      , catMaybes [toNativeAssetQuantity offerAsset', toNativeAssetQuantity askAsset']
      )

    (newDatum :: Datum , beacons :: [NativeAsset]) =
      case swapDatum of
        Just (OneWayDatum d@OneWay.SwapDatum{..}) -> 
          let reqBeacons =
                -- One trading pair beacon.
                [ NativeAsset beaconId pairBeacon "" 1
                -- One offer beacon.
                , NativeAsset beaconId offerBeacon "" 1
                -- One Ask beacon.
                , NativeAsset beaconId askBeacon "" 1
                ]
          in (toDatum $ d & #prevInput ?~ utxoRef, reqBeacons)

        Just (TwoWayDatum d@TwoWay.SwapDatum{..}) -> 
          let reqBeacons =
                -- One trading pair beacon.
                [ NativeAsset beaconId pairBeacon "" 1
                -- One asset1 beacon.
                , NativeAsset beaconId asset1Beacon "" 1
                -- One asset2 beacon.
                , NativeAsset beaconId asset2Beacon "" 1
                ]
          in (toDatum $ d & #prevInput ?~ utxoRef, reqBeacons)

        -- This should never happen.
        -- TODO: Find a better way to handle this.
        _ -> error "Invalid swap added to TxBody"

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = swapAddress
      , lovelace = newLovelace
      , nativeAssets = newNativeAssets <> beacons -- Add the beacons to the output.
      , datum = OutputDatum newDatum
      }

-- | All `add` functions assume `SpendWithMint` will be used with a minting execution. If no
-- minting is required, then the spending redeemers must be changed to `SpendWithStake` and
-- a staking execution must be added for the beacon script.
adjustSpendingRedeemers :: TxBody -> TxBody
adjustSpendingRedeemers txBody@TxBody{mints,inputs} = 
    txBody
      & adjustOneWaySpendingRedeemers
      & adjustTwoWaySpendingRedeemers
  where
    adjustOneWaySpendingRedeemers :: TxBody -> TxBody
    adjustOneWaySpendingRedeemers body
      | hasOneWayInput && isNothing oneWayMint = 
          body
            & #inputs %~ map (changeRedeemer OneWay.swapScriptHash (toRedeemer OneWay.SpendWithStake))
            & #withdrawals %~ (oneWayStakeWithdrawal:)
      | otherwise = body

    adjustTwoWaySpendingRedeemers :: TxBody -> TxBody
    adjustTwoWaySpendingRedeemers body
      | hasTwoWayInput && isNothing twoWayMint =
          body
            & #inputs %~ map (changeRedeemer TwoWay.swapScriptHash (toRedeemer TwoWay.SpendWithStake))
            & #withdrawals %~ (twoWayStakeWithdrawal:)
      | otherwise = body

    hasOneWayInput :: Bool
    hasOneWayInput = flip any inputs $ \TxBodyInput{..} ->
      maybe False ((==OneWay.swapScriptHash) . view #scriptHash) spendingScriptInfo
      
    hasTwoWayInput :: Bool
    hasTwoWayInput = flip any inputs $ \TxBodyInput{..} ->
      maybe False ((==TwoWay.swapScriptHash) . view #scriptHash) spendingScriptInfo

    oneWayMint :: Maybe TxBodyMint
    oneWayMint = find ((== OneWay.beaconScriptHash) . view #mintingPolicyHash) mints

    twoWayMint :: Maybe TxBodyMint
    twoWayMint = find ((== TwoWay.beaconScriptHash) . view #mintingPolicyHash) mints

    changeRedeemer :: ScriptHash -> Redeemer -> TxBodyInput -> TxBodyInput
    changeRedeemer targetHash newRedeemer input@TxBodyInput{spendingScriptInfo} =
      case spendingScriptInfo of
        Nothing -> input
        Just SpendingScriptInfo{scriptHash} ->
          if targetHash == scriptHash then
            input & #spendingScriptInfo % _Just % #redeemer .~ newRedeemer
          else
            input

    oneWayStakeWithdrawal :: TxBodyWithdrawal
    oneWayStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = OneWay.beaconStakeAddress Testnet
      , lovelace = 0
      , stakeCredential = ScriptCredential OneWay.beaconScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness OneWay.beaconScriptTestnetRef
          , redeemer = toRedeemer OneWay.UpdateSwaps
          , executionBudget = def
          }
      }

    twoWayStakeWithdrawal :: TxBodyWithdrawal
    twoWayStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = TwoWay.beaconStakeAddress Testnet
      , lovelace = 0
      , stakeCredential = ScriptCredential TwoWay.beaconScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness TwoWay.beaconScriptTestnetRef
          , redeemer = toRedeemer TwoWay.UpdateSwaps
          , executionBudget = def
          }
      }
