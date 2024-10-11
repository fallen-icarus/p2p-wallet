{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel
  ( AftermarketBuilderModel(..)
  , emptyAftermarketBuilderModel
  , isEmptyAftermarketBuilderModel

  , AftermarketBuilderEvent(..)

  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleUpdate
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleClose
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Aftermarket Builder Events
-------------------------------------------------
-- | All events dealing with the aftermarket. This is separate to help stay organized.
data AftermarketBuilderEvent
  -- | Remove the selected sale creation from the builder.
  = RemoveSelectedSaleCreation Int
  -- | Edit selected sale creation.
  | EditSelectedSaleCreation (AddEvent (Int,SaleCreation) (Int,SaleCreation))
  -- | Remove the selected NFT from the SaleCreation.
  | RemoveSaleCreationNft NativeAsset
  -- | Remove the selected sale close from the builder.
  | RemoveSelectedSaleClose Int
  -- | Remove the selected sale update from the builder.
  | RemoveSelectedSaleUpdate Int
  -- | Edit selected sale update.
  | EditSelectedSaleUpdate (AddEvent (Int,SaleUpdate) (Int,SaleCreation))
  -- | Remove the selected NFT from the SaleUpdate.
  | RemoveSaleUpdateNft NativeAsset
  deriving (Show,Eq)

-------------------------------------------------
-- Aftermarket Builder Model
-------------------------------------------------
-- | Due to the number of possible aftermarket actions, they are grouped in this sub-model to
-- help the code stay organized.
data AftermarketBuilderModel = AftermarketBuilderModel
  { saleCreations :: [(Int,SaleCreation)]
  , targetSaleCreation :: Maybe (Int,NewSaleCreation)
  , saleCloses :: [(Int,SaleClose)]
  , saleUpdates :: [(Int,SaleUpdate)]
  , targetSaleUpdate :: Maybe (Int,NewSaleCreation)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketBuilderModel

instance Default AftermarketBuilderModel where
  def = AftermarketBuilderModel
    { saleCreations = []
    , targetSaleCreation = Nothing
    , saleCloses = []
    , saleUpdates = []
    , targetSaleUpdate = Nothing
    }

-- | This is just an alias for `def`. The name is more clear what it is.
emptyAftermarketBuilderModel :: AftermarketBuilderModel
emptyAftermarketBuilderModel = def

isEmptyAftermarketBuilderModel :: AftermarketBuilderModel -> Bool
isEmptyAftermarketBuilderModel AftermarketBuilderModel{..} = and
  [ null saleCreations
  , null saleCloses
  , null saleUpdates
  , isNothing targetSaleCreation
  , isNothing targetSaleUpdate
  ]

-------------------------------------------------
-- AftermarketBuilderModel --> TxBody
-------------------------------------------------
-- Converting the `AftermarketBuilderModel` to a `TxBody`. 
instance AddToTxBody AftermarketBuilderModel where
  addToTxBody txBody AftermarketBuilderModel{..} = 
      txBody
        -- Add the sale creations.
        & flip (foldl' addSaleCreationToBody) saleCreations
        -- Add the sale closes.
        & flip (foldl' addSaleCloseToBody) saleCloses
        -- Add the sale updates.
        & flip (foldl' addSaleUpdateToBody) saleUpdates
        -- Merge any beacon mints so that there is only one `TxBodyMint` per minting policy.
        & #mints %~ mergeTxBodyMints
        -- Adjust any executions based on whether all mints canceled out.
        & adjustBeaconExecution
        -- Remove empty mints.
        & #mints %~ removeEmptyMints
        -- Remove duplicate stake withdrawals. This removes duplicate observer entries.
        & #withdrawals %~ ordNubOn (view #stakeCredential)

addSaleCreationToBody :: TxBody -> (Int,SaleCreation) -> TxBody
addSaleCreationToBody txBody (_,SaleCreation{..}) =
    txBody 
      & #outputs %~ flip snoc newOutput
      & #mints %~ (newMint:)
      & #network .~ network
  where
    saleDatum
      | isAuction = 
          AuctionDatum $ Aftermarket.createAuctionDatum nfts salePrice
      | otherwise = 
          SpotDatum $ Aftermarket.createSpotDatum nfts (paymentWallet ^. #paymentAddress) deposit salePrice

    categoryBeacon
      | isAuction = Aftermarket.auctionBeaconName
      | otherwise = Aftermarket.spotBeaconName

    policyBeacon 
      | isAuction = maybe "" Aftermarket.genPolicyBeacon $ saleDatum ^? _AuctionDatum % #nftPolicyId
      | otherwise = maybe "" Aftermarket.genPolicyBeacon $ saleDatum ^? _SpotDatum % #nftPolicyId

    beacons = 
      -- One Spot Beacon or Auction Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol categoryBeacon "" 1
      -- One PolicyBeacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol (policyBeacon ^. #unPolicyBeacon) "" 1
      ]

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = marketWallet ^. #marketAddress
      , lovelace = deposit
      , nativeAssets = nfts <> beacons -- Add the beacons to the output.
      , datum = OutputDatum $
          if isAuction 
          then toDatum $ fromMaybe def $ saleDatum ^? _AuctionDatum
          else toDatum $ fromMaybe def $ saleDatum ^? _SpotDatum
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = Aftermarket.beaconScriptHash
      , nativeAssets = beacons
      , redeemer = toRedeemer Aftermarket.CreateCloseOrUpdateMarketUTxOs
      , scriptWitness = 
          ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.beaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

-- | If beacons do not need to be minted/burned, the beacon script should be executed as
-- a staking script instead.
adjustBeaconExecution :: TxBody -> TxBody
adjustBeaconExecution txBody@TxBody{network,mints}
  | isBeaconRedeemer && beacons == Just [] = txBody & #withdrawals %~ (beaconStakeWithdrawal:)
  | otherwise = txBody
  where
    beaconMint :: Maybe TxBodyMint
    beaconMint = find ((== Aftermarket.beaconScriptHash) . view #mintingPolicyHash) mints

    beacons :: Maybe [NativeAsset]
    beacons = view #nativeAssets <$> beaconMint

    beaconRedeemer :: Maybe Aftermarket.BeaconsRedeemer
    beaconRedeemer = 
      fmap (view #redeemer) beaconMint >>= fromRedeemer @Aftermarket.BeaconsRedeemer

    isBeaconRedeemer :: Bool
    isBeaconRedeemer = maybe False (is Aftermarket._CreateCloseOrUpdateMarketUTxOs) beaconRedeemer

    mintInfoToStakingInfo :: TxBodyMint -> StakingScriptInfo
    mintInfoToStakingInfo TxBodyMint{..} = StakingScriptInfo
      { scriptWitness = scriptWitness
      , redeemer = redeemer
      , executionBudget = executionBudget
      }

    beaconStakeWithdrawal :: TxBodyWithdrawal
    beaconStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.beaconStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.beaconScriptHash
      , stakingScriptInfo = mintInfoToStakingInfo <$> beaconMint
      }

addSaleCloseToBody :: TxBody -> (Int,SaleClose) -> TxBody
addSaleCloseToBody txBody (_,SaleClose{..}) =
    txBody
      -- Add the new sale to the inputs.
      & #inputs %~ flip snoc newInput
      -- Add the new beacons to be burned. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      & #network .~ network
  where
    beacons = case marketDatum of
      Just (SpotDatum Aftermarket.SpotDatum{nftPolicyId}) ->
        let policyBeacon = Aftermarket.genPolicyBeacon nftPolicyId ^. #unPolicyBeacon in
        -- One Spot Beacon.
        [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.spotBeaconName "" (-1)
        -- One Policy Beacon.
        , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
        ]
      Just (AuctionDatum Aftermarket.AuctionDatum{nftPolicyId}) ->
        let policyBeacon = Aftermarket.genPolicyBeacon nftPolicyId ^. #unPolicyBeacon in
        -- One Auction Beacon.
        [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.auctionBeaconName "" (-1)
        -- One Policy Beacon.
        , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
        ]
      _ -> []

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case sellerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.CloseOrUpdateSellerUTxO
          , scriptWitness = 
              ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.aftermarketScriptHash
          , executionBudget = def
          , scriptHash = Aftermarket.aftermarketScriptHash
          }
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Aftermarket.beaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = toRedeemer Aftermarket.CreateCloseOrUpdateMarketUTxOs
      , scriptWitness = 
          ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.beaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

addSaleUpdateToBody :: TxBody -> (Int,SaleUpdate) -> TxBody
addSaleUpdateToBody txBody (idx,SaleUpdate{..}) =
  txBody
    & flip addSaleCreationToBody (idx,newSale)
    & flip addSaleCloseToBody (idx,oldSale)
