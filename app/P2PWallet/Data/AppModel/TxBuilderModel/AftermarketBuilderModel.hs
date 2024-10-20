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

  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.AcceptedBidClaim
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUnlock
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUpdate
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.ClaimBidAcceptance
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeyAcceptedBidClaim
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeySpotPurchase
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeyAcceptedBidClaim
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeySpotPurchase
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleUpdate
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotBidAcceptance
  , module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotPurchase
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.AcceptedBidClaim
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidClose
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidCreation
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUnlock
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidUpdate
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.ClaimBidAcceptance
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeyAcceptedBidClaim
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.LoanKeySpotPurchase
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeyAcceptedBidClaim
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeySpotPurchase
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleClose
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleUpdate
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotBidAcceptance
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotPurchase
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
  -- | Remove the selected loan key spot purchase from the builder.
  | RemoveSelectedLoanKeySpotPurchase Int
  -- | Edit selected loan key spot purchase.
  | EditSelectedLoanKeySpotPurchase (AddEvent (Int,LoanKeySpotPurchase) (Int,LoanKeySpotPurchase))
  -- | Remove the selected bid creation from the builder.
  | RemoveSelectedBidCreation Int
  -- | Edit selected bid creation.
  | EditSelectedBidCreation (AddEvent (Int,BidCreation) (Int,BidCreation))
  -- | Remove the selected NFT from the BidCreation.
  | RemoveBidCreationNft NativeAsset
  -- | Remove the selected bid close from the builder.
  | RemoveSelectedBidClose Int
  -- | Remove the selected bid update from the builder.
  | RemoveSelectedBidUpdate Int
  -- | Edit selected bid update.
  | EditSelectedBidUpdate (AddEvent (Int,BidUpdate) (Int,BidCreation))
  -- | Remove the selected NFT from the BidUpdate.
  | RemoveBidUpdateNft NativeAsset
  -- | Remove the selected claim bid acceptance from the builder.
  | RemoveSelectedClaimBidAcceptance Int
  -- | Edit selected claim bid acceptance.
  | EditSelectedClaimBidAcceptance (AddEvent (Int,ClaimBidAcceptance) (Int,ClaimBidAcceptance))
  -- | Remove the selected loan key bid claim from the builder.
  | RemoveSelectedLoanKeyBidClaim Int
  -- | Edit selected loan key bid claim.
  | EditSelectedLoanKeyBidClaim (AddEvent (Int,LoanKeyAcceptedBidClaim) (Int,LoanKeyAcceptedBidClaim))
  -- | Remove the selected options key spot purchase from the builder.
  | RemoveSelectedOptionsKeySpotPurchase Int
  -- | Edit selected options key spot purchase.
  | EditSelectedOptionsKeySpotPurchase (AddEvent (Int,OptionsKeySpotPurchase) (Int,OptionsKeySpotPurchase))
  -- | Remove the selected spot bid acceptance from the builder.
  | RemoveSelectedSpotBidAcceptance Int
  -- | Remove the selected options key bid claim from the builder.
  | RemoveSelectedOptionsKeyBidClaim Int
  -- | Edit selected options key bid claim.
  | EditSelectedOptionsKeyBidClaim (AddEvent (Int,OptionsKeyAcceptedBidClaim) (Int,OptionsKeyAcceptedBidClaim))
  -- | Remove the selected bid unlock from the builder.
  | RemoveSelectedBidUnlock Int
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
  , loanKeySpotPurchases :: [(Int,LoanKeySpotPurchase)]
  , targetLoanKeySpotPurchase :: Maybe (Int,NewLoanKeySpotPurchase)
  , bidCreations :: [(Int,BidCreation)]
  , targetBidCreation :: Maybe (Int,NewBidCreation)
  , bidCloses :: [(Int,BidClose)]
  , bidUpdates :: [(Int,BidUpdate)]
  , targetBidUpdate :: Maybe (Int,NewBidCreation)
  , claimBidAcceptances :: [(Int,ClaimBidAcceptance)]
  , targetClaimBidAcceptance :: Maybe (Int,ClaimBidAcceptance)
  , loanKeyBidClaims :: [(Int,LoanKeyAcceptedBidClaim)]
  , targetLoanKeyBidClaim :: Maybe (Int,NewLoanKeyAcceptedBidClaim)
  , optionsKeySpotPurchases :: [(Int,OptionsKeySpotPurchase)]
  , targetOptionsKeySpotPurchase :: Maybe (Int,NewOptionsKeySpotPurchase)
  , spotBidAcceptances :: [(Int,SpotBidAcceptance)]
  , optionsKeyBidClaims :: [(Int,OptionsKeyAcceptedBidClaim)]
  , targetOptionsKeyBidClaim :: Maybe (Int,NewOptionsKeyAcceptedBidClaim)
  , bidUnlocks :: [(Int,BidUnlock)]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketBuilderModel

instance Default AftermarketBuilderModel where
  def = AftermarketBuilderModel
    { saleCreations = []
    , targetSaleCreation = Nothing
    , saleCloses = []
    , saleUpdates = []
    , targetSaleUpdate = Nothing
    , loanKeySpotPurchases = []
    , targetLoanKeySpotPurchase = Nothing
    , bidCreations = []
    , targetBidCreation = Nothing
    , bidCloses = []
    , bidUpdates = []
    , targetBidUpdate = Nothing
    , claimBidAcceptances = []
    , targetClaimBidAcceptance = Nothing
    , loanKeyBidClaims = []
    , targetLoanKeyBidClaim = Nothing
    , optionsKeySpotPurchases = []
    , targetOptionsKeySpotPurchase = Nothing
    , spotBidAcceptances = []
    , optionsKeyBidClaims = []
    , targetOptionsKeyBidClaim = Nothing
    , bidUnlocks = []
    }

-- | This is just an alias for `def`. The name is more clear what it is.
emptyAftermarketBuilderModel :: AftermarketBuilderModel
emptyAftermarketBuilderModel = def

isEmptyAftermarketBuilderModel :: AftermarketBuilderModel -> Bool
isEmptyAftermarketBuilderModel AftermarketBuilderModel{..} = and
  [ null saleCreations
  , null saleCloses
  , null saleUpdates
  , null loanKeySpotPurchases
  , null bidCreations
  , null bidCloses
  , null bidUpdates
  , null claimBidAcceptances
  , null loanKeyBidClaims
  , null optionsKeySpotPurchases
  , null spotBidAcceptances
  , null optionsKeyBidClaims
  , null bidUnlocks
  , isNothing targetSaleCreation
  , isNothing targetSaleUpdate
  , isNothing targetLoanKeySpotPurchase
  , isNothing targetBidCreation
  , isNothing targetBidUpdate
  , isNothing targetClaimBidAcceptance
  , isNothing targetLoanKeyBidClaim
  , isNothing targetOptionsKeySpotPurchase
  , isNothing targetOptionsKeyBidClaim
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
        -- Add the loan key spot purchases. This only adds the spot purchase actions. The lender
        -- address updates and expired loan claims are handled by the LoanBuilderModel.
        & flip (foldl' addLoanKeySpotPurchase) loanKeySpotPurchases
        -- Add the options key spot purchases. This only adds the spot purchase actions. The
        -- executions are handled by the OptionsBuilderModel.
        & flip (foldl' addOptionsKeySpotPurchase) optionsKeySpotPurchases
        -- Add the bid creations.
        & flip (foldl' addBidCreationToBody) bidCreations
        -- Add the bid closes.
        & flip (foldl' addBidCloseToBody) bidCloses
        -- Add the bid updates.
        & flip (foldl' addBidUpdateToBody) bidUpdates
        -- Add the claim bid acceptances.
        & flip (foldl' addClaimBidAcceptanceToBody) claimBidAcceptances
        -- Add the loan key accepted bid claims. This only adds the bid claim actions. The lender
        -- address updates and expired loan claims are handled by the LoanBuilderModel.
        & flip (foldl' addLoanKeyAcceptedBidClaimToBody) loanKeyBidClaims
        -- Add the options key accepted bid claims. This only adds the bid claim actions. Contract
        -- executions will be handled by the OptionsBuilderModel.
        & flip (foldl' addOptionsKeyAcceptedBidClaimToBody) optionsKeyBidClaims
        -- Add the spot bid acceptances.
        & flip (foldl' addSpotBidAcceptanceToBody) spotBidAcceptances
        -- Add the bid unlocks.
        & flip (foldl' addBidUnlockToBody) bidUnlocks
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
      _ -> error "SaleClose UTxO has wrong datum type"

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

addSpotPurchaseToBody :: TxBody -> (Int,SpotPurchase) -> TxBody
addSpotPurchaseToBody txBody (_,SpotPurchase{..}) = 
    txBody
      -- Add the spot input to the list of inputs.
      & #inputs %~ (<> [newSpotInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newPaymentOutput])
      -- Add the new beacons to be burned. 
      & #mints %~ (<> [newBeaconBurn])
      -- Add the aftermarket observer execution.
      & #withdrawals %~ (aftermarketObserverWithdrawal:)
      & #network .~ network

  where
    Aftermarket.SpotDatum{nftPolicyId,salePrice,paymentAddress,saleDeposit} = 
      case saleUTxO ^. #marketDatum of
        Just (SpotDatum spotDatum) -> spotDatum
        _ -> error "SpotPurchase UTxO has wrong datum type"

    price = map toNativeAsset $ Aftermarket.unPrices salePrice

    beacons = 
      let policyBeacon = Aftermarket.genPolicyBeacon nftPolicyId ^. #unPolicyBeacon in
      -- One Spot Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.spotBeaconName "" (-1)
      -- One Policy Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
      ]

    sellerPaymentAddress :: PaymentAddress
    sellerPaymentAddress = either (const "") fst
                         $ plutusToBech32 network paymentAddress

    lovelace = 
      let bidLoves = maybe 0 (view #quantity) $ find ((=="") . view #policyId) price 
       in extraPaymentDeposit + Lovelace (bidLoves + saleDeposit)

    nativeAssets = filter ((/="") . view #policyId) price

    newSpotInput :: TxBodyInput
    newSpotInput = TxBodyInput
      { utxoRef = saleUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.PurchaseSpot
          , scriptWitness = 
              ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.aftermarketScriptHash
          , executionBudget = def
          , scriptHash = Aftermarket.aftermarketScriptHash
          }
      }

    newPaymentOutput :: TxBodyOutput
    newPaymentOutput = TxBodyOutput
      { paymentAddress = sellerPaymentAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , datum = OutputDatum $ toDatum $ Aftermarket.PaymentDatum
          ( Aftermarket.beaconCurrencySymbol
          , saleUTxO ^. #utxoRef
          )
      }

    newBeaconBurn :: TxBodyMint
    newBeaconBurn = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Aftermarket.beaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = 
          -- BurnBeacons would be cheaper, but this redeemer is easier to work with when
          -- creating composed transactions.
          toRedeemer Aftermarket.CreateCloseOrUpdateMarketUTxOs
      , scriptWitness = ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.beaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

    aftermarketObserverWithdrawal :: TxBodyWithdrawal
    aftermarketObserverWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.observerStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.aftermarketObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ 
              Aftermarket.getScriptRef network Aftermarket.aftermarketObserverScriptHash
          , redeemer = toRedeemer 
                     $ Aftermarket.ObserveAftermarket 
                     $ Aftermarket.BeaconId Aftermarket.beaconCurrencySymbol
          , executionBudget = def
          }
      }

addLoanKeySpotPurchase :: TxBody -> (Int,LoanKeySpotPurchase) -> TxBody
addLoanKeySpotPurchase txBody (idx,LoanKeySpotPurchase{spotPurchase}) =
  txBody
    & flip addSpotPurchaseToBody (idx,spotPurchase)

addOptionsKeySpotPurchase :: TxBody -> (Int,OptionsKeySpotPurchase) -> TxBody
addOptionsKeySpotPurchase txBody (idx,OptionsKeySpotPurchase{spotPurchase}) =
  txBody
    & flip addSpotPurchaseToBody (idx,spotPurchase)

addLoanKeyAcceptedBidClaimToBody :: TxBody -> (Int,LoanKeyAcceptedBidClaim) -> TxBody
addLoanKeyAcceptedBidClaimToBody txBody (idx,LoanKeyAcceptedBidClaim{bidClaim}) =
  txBody
    & flip addAcceptedBidClaimToBody (idx,bidClaim)

addOptionsKeyAcceptedBidClaimToBody :: TxBody -> (Int,OptionsKeyAcceptedBidClaim) -> TxBody
addOptionsKeyAcceptedBidClaimToBody txBody (idx,OptionsKeyAcceptedBidClaim{bidClaim}) =
  txBody
    & flip addAcceptedBidClaimToBody (idx,bidClaim)

addBidCreationToBody :: TxBody -> (Int,BidCreation) -> TxBody
addBidCreationToBody txBody (_,BidCreation{..}) =
    txBody 
      & #outputs %~ flip snoc newOutput
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      -- Invalid hereafter must be set to the expiration when creating ClaimBids.
      & #invalidHereafter %~ updateInvalidHereafter upperBound
      & #network .~ network
  where
    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    expiration :: PlutusTime
    expiration = toPlutusTime $ fromMaybe claimExpiration bidExpiration

    upperBound :: Maybe Slot
    upperBound
      | isSpotBid = Nothing
      | otherwise = Just $
          -- The node can only specify invalidHereafter for 1.5 days into the future due to the
          -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
          -- change the behavior of the protocol.
          min (posixTimeToSlot slotConfig expiration) (129600 + posixTimeToSlot slotConfig currentTime)

    MarketWallet{stakeCredential,stakeKeyDerivation} = marketWallet

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    bidDatum
      | isSpotBid =
          SpotBidDatum $
            Aftermarket.createSpotBidDatum 
              nfts 
              deposit 
              (paymentWallet ^. #paymentAddress)
              bid 
              stakeCredential
      | otherwise = 
          ClaimBidDatum $
            Aftermarket.createClaimBidDatum 
              nfts 
              deposit 
              bid 
              (toPlutusTime <$> bidExpiration) 
              (toPlutusTime claimExpiration)
              stakeCredential

    policyBeacon 
      | isSpotBid = maybe "" Aftermarket.genPolicyBeacon $ bidDatum ^? _SpotBidDatum % #nftPolicyId
      | otherwise = maybe "" Aftermarket.genPolicyBeacon $ bidDatum ^? _ClaimBidDatum % #nftPolicyId

    bidderId = Aftermarket.genBidderId stakeCredential

    beacons = 
      -- One Bid Beacon or Auction Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol "Bid" "" 1
      -- One PolicyBeacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol (policyBeacon ^. #unPolicyBeacon) "" 1
      -- One BidderId.
      , NativeAsset Aftermarket.beaconCurrencySymbol (bidderId ^. #unBidderId) "" 1
      ]

    bidLoves = maybe 0 (Lovelace . view #quantity) $ find (\NativeAsset{policyId} -> policyId == "") bid
    bidAssets = filter (\NativeAsset{policyId} -> policyId /= "") bid

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = sellerAddress
      , lovelace = 
          if isSpotBid
          then deposit + bidLoves
          else deposit
      , nativeAssets = 
          if isSpotBid
          then bidAssets <> beacons
          else beacons
      , datum = OutputDatum $
          if isSpotBid 
          then toDatum $ fromMaybe def $ bidDatum ^? _SpotBidDatum
          else toDatum $ fromMaybe def $ bidDatum ^? _ClaimBidDatum
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

addBidCloseToBody :: TxBody -> (Int,BidClose) -> TxBody
addBidCloseToBody txBody (_,BidClose{..}) =
    txBody
      -- Add the new bid to the inputs.
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
    bidderId = Aftermarket.unBidderId $ Aftermarket.genBidderId bidderCredential
    policyBeacon = maybe "" (Aftermarket.unPolicyBeacon . Aftermarket.genPolicyBeacon . fst) 
                 $ marketDatum >>= aftermarketDatumNfts
    beacons = 
      -- One Bid Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.bidBeaconName "" (-1)
      -- One Policy Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
      -- One BidderId.
      , NativeAsset Aftermarket.beaconCurrencySymbol bidderId "" (-1)
      ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case bidderCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.CloseOrUpdateBidderUTxO
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

addBidUpdateToBody :: TxBody -> (Int,BidUpdate) -> TxBody
addBidUpdateToBody txBody (idx,BidUpdate{..}) =
  txBody
    & flip addBidCreationToBody (idx,newBid)
    & flip addBidCloseToBody (idx,oldBid)

addClaimBidAcceptanceToBody :: TxBody -> (Int,ClaimBidAcceptance) -> TxBody
addClaimBidAcceptanceToBody txBody (_,ClaimBidAcceptance{..}) = 
    txBody
      -- Add the bid input to the list of inputs.
      & #inputs %~ (<> [newBidInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newBidOutput])
      -- Add the aftermarket observer execution.
      & #withdrawals %~ (aftermarketObserverWithdrawal:)
      & #network .~ network
      -- Invalid hereafter must be set to the bid expiration when accepting ClaimBids.
      & #invalidHereafter %~ updateInvalidHereafter upperBound
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
  where
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case marketWallet ^. #stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, marketWallet ^. #stakeKeyDerivation)

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    claimBidDatum@Aftermarket.ClaimBidDatum{nftPolicyId,nftNames,bidExpiration} = 
      case bidUTxO ^. #marketDatum of
        Just (ClaimBidDatum datum) -> datum
        _ -> error "ClaimBidAcceptance UTxO has wrong datum type"

    acceptedBidDatum = 
      Aftermarket.createAcceptedBidDatum claimBidDatum extraDeposit $ paymentWallet ^. #paymentAddress

    upperBound :: Maybe Slot
    upperBound = flip (maybe Nothing) bidExpiration $ \expr ->
      -- The node can only specify invalidHereafter for 1.5 days into the future due to the
      -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
      -- change the behavior of the protocol.
      Just $ min (posixTimeToSlot slotConfig expr) (129600 + posixTimeToSlot slotConfig currentTime)

    lovelace = bidUTxO ^. #lovelace + extraDeposit

    nativeAssets = sumNativeAssets $ concat
      [ bidUTxO ^. #nativeAssets
      , map (set #quantity 1 . mkNativeAsset nftPolicyId) nftNames
      ]

    newBidInput :: TxBodyInput
    newBidInput = TxBodyInput
      { utxoRef = bidUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer 
                     $ Aftermarket.AcceptClaimBid (unLovelace extraDeposit) 
                     $ acceptedBidDatum ^. #paymentAddress
          , scriptWitness = 
              ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.aftermarketScriptHash
          , executionBudget = def
          , scriptHash = Aftermarket.aftermarketScriptHash
          }
      }

    newBidOutput :: TxBodyOutput
    newBidOutput = TxBodyOutput
      { paymentAddress = bidUTxO ^. #marketAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , datum = OutputDatum $ toDatum acceptedBidDatum
      }

    aftermarketObserverWithdrawal :: TxBodyWithdrawal
    aftermarketObserverWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.observerStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.aftermarketObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ 
              Aftermarket.getScriptRef network Aftermarket.aftermarketObserverScriptHash
          , redeemer = toRedeemer 
                     $ Aftermarket.ObserveAftermarket 
                     $ Aftermarket.BeaconId Aftermarket.beaconCurrencySymbol
          , executionBudget = def
          }
      }

addAcceptedBidClaimToBody :: TxBody -> (Int,AcceptedBidClaim) -> TxBody
addAcceptedBidClaimToBody txBody (_,AcceptedBidClaim{..}) = 
    txBody
      -- Add the bid input to the list of inputs.
      & #inputs %~ (<> [newBidInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newPaymentOutput])
      -- Add the new beacons to be burned. 
      & #mints %~ (<> [newBeaconBurn])
      -- Add the aftermarket observer execution.
      & #withdrawals %~ (aftermarketObserverWithdrawal:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      & #network .~ network

  where
    MarketWallet{stakeCredential,stakeKeyDerivation} = marketWallet

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    Aftermarket.AcceptedBidDatum{nftPolicyId,bid,paymentAddress,sellerDeposit} = 
      case bidUTxO ^. #marketDatum of
        Just (AcceptedBidDatum datum) -> datum
        _ -> error "AcceptedBid UTxO has wrong datum type"

    bidderId = Aftermarket.unBidderId $ Aftermarket.genBidderId stakeCredential
    policyBeacon = Aftermarket.unPolicyBeacon $ Aftermarket.genPolicyBeacon nftPolicyId
    price = map toNativeAsset $ Aftermarket.unPrices bid

    beacons = 
      -- One Bid Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.bidBeaconName "" (-1)
      -- One Policy Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
      -- One BidderId.
      , NativeAsset Aftermarket.beaconCurrencySymbol bidderId "" (-1)
      ]

    sellerPaymentAddress :: PaymentAddress
    sellerPaymentAddress = either (const "") fst $ plutusToBech32 network paymentAddress

    lovelace = 
      let bidLoves = maybe 0 (view #quantity) $ find ((=="") . view #policyId) price 
       in extraPaymentDeposit + Lovelace (bidLoves + sellerDeposit)

    nativeAssets = filter ((/="") . view #policyId) price

    newBidInput :: TxBodyInput
    newBidInput = TxBodyInput
      { utxoRef = bidUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.ClaimAcceptedBid
          , scriptWitness = 
              ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.aftermarketScriptHash
          , executionBudget = def
          , scriptHash = Aftermarket.aftermarketScriptHash
          }
      }

    newPaymentOutput :: TxBodyOutput
    newPaymentOutput = TxBodyOutput
      { paymentAddress = sellerPaymentAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , datum = OutputDatum $ toDatum $ Aftermarket.PaymentDatum
          ( Aftermarket.beaconCurrencySymbol
          , bidUTxO ^. #utxoRef
          )
      }

    newBeaconBurn :: TxBodyMint
    newBeaconBurn = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Aftermarket.beaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = 
          -- BurnBeacons would be cheaper, but this redeemer is easier to work with when
          -- creating composed transactions.
          toRedeemer Aftermarket.CreateCloseOrUpdateMarketUTxOs
      , scriptWitness = ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.beaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

    aftermarketObserverWithdrawal :: TxBodyWithdrawal
    aftermarketObserverWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.observerStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.aftermarketObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ 
              Aftermarket.getScriptRef network Aftermarket.aftermarketObserverScriptHash
          , redeemer = toRedeemer 
                     $ Aftermarket.ObserveAftermarket 
                     $ Aftermarket.BeaconId Aftermarket.beaconCurrencySymbol
          , executionBudget = def
          }
      }

addSpotBidAcceptanceToBody :: TxBody -> (Int,SpotBidAcceptance) -> TxBody
addSpotBidAcceptanceToBody txBody (_,SpotBidAcceptance{..}) = 
    txBody
      -- Add the bid input to the list of inputs.
      & #inputs %~ (<> [newBidInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newPaymentOutput])
      -- Add the new beacons to be burned. 
      & #mints %~ (<> [newBeaconBurn])
      -- Add the aftermarket observer execution.
      & #withdrawals %~ (aftermarketObserverWithdrawal:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      & #network .~ network
  where
    Aftermarket.SpotBidDatum{bidderCredential,nftPolicyId,nftNames,paymentAddress,bidDeposit} = 
      case bidUTxO ^. #marketDatum of
        Just (SpotBidDatum spotBidDatum) -> spotBidDatum
        _ -> error "SpotBidAcceptance UTxO has wrong datum type"

    nfts = map (set #quantity 1 . mkNativeAsset nftPolicyId) nftNames
    policyBeacon = Aftermarket.genPolicyBeacon nftPolicyId ^. #unPolicyBeacon
    bidderId = Aftermarket.genBidderId bidderCredential ^. #unBidderId

    beacons = 
      -- One Bid Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.bidBeaconName "" (-1)
      -- One Policy Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
      -- One BidderId Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol bidderId "" (-1)
      ]

    buyerPaymentAddress :: PaymentAddress
    buyerPaymentAddress = either (const "") fst
                        $ plutusToBech32 network paymentAddress

    lovelace = Lovelace bidDeposit

    nativeAssets = nfts

    MarketWallet{stakeCredential,stakeKeyDerivation} = marketWallet

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newBidInput :: TxBodyInput
    newBidInput = TxBodyInput
      { utxoRef = bidUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.AcceptSpotBid
          , scriptWitness = 
              ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.aftermarketScriptHash
          , executionBudget = def
          , scriptHash = Aftermarket.aftermarketScriptHash
          }
      }

    newPaymentOutput :: TxBodyOutput
    newPaymentOutput = TxBodyOutput
      { paymentAddress = buyerPaymentAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , datum = OutputDatum $ toDatum $ Aftermarket.PaymentDatum
          ( Aftermarket.beaconCurrencySymbol
          , bidUTxO ^. #utxoRef
          )
      }

    newBeaconBurn :: TxBodyMint
    newBeaconBurn = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Aftermarket.beaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = 
          -- BurnBeacons would be cheaper, but this redeemer is easier to work with when
          -- creating composed transactions.
          toRedeemer Aftermarket.CreateCloseOrUpdateMarketUTxOs
      , scriptWitness = ReferenceWitness $ Aftermarket.getScriptRef network Aftermarket.beaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

    aftermarketObserverWithdrawal :: TxBodyWithdrawal
    aftermarketObserverWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.observerStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.aftermarketObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ 
              Aftermarket.getScriptRef network Aftermarket.aftermarketObserverScriptHash
          , redeemer = toRedeemer 
                     $ Aftermarket.ObserveAftermarket 
                     $ Aftermarket.BeaconId Aftermarket.beaconCurrencySymbol
          , executionBudget = def
          }
      }

addBidUnlockToBody :: TxBody -> (Int,BidUnlock) -> TxBody
addBidUnlockToBody txBody (_,BidUnlock{..}) =
    txBody
      -- Add the new bid to the inputs.
      & #inputs %~ flip snoc newInput
      -- Add the new beacons to be burned. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      -- Set invalid-before to the claim expiration.
      & #invalidBefore %~ updateInvalidBefore (Just lowerBound)
      -- Add the aftermarket observer execution.
      & #withdrawals %~ (aftermarketObserverWithdrawal:)
      & #network .~ network
  where
    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    Aftermarket.AcceptedBidDatum{nftPolicyId,bidderCredential,claimExpiration} = 
      case bidUTxO ^. #marketDatum of
        Just (AcceptedBidDatum acceptedBidDatum) -> acceptedBidDatum
        _ -> error "BidUnlock UTxO has wrong datum type"

    lowerBound :: Slot
    lowerBound = posixTimeToSlot slotConfig claimExpiration

    bidderId = Aftermarket.unBidderId $ Aftermarket.genBidderId bidderCredential
    policyBeacon = Aftermarket.unPolicyBeacon $ Aftermarket.genPolicyBeacon nftPolicyId
    beacons = 
      -- One Bid Beacon.
      [ NativeAsset Aftermarket.beaconCurrencySymbol Aftermarket.bidBeaconName "" (-1)
      -- One Policy Beacon.
      , NativeAsset Aftermarket.beaconCurrencySymbol policyBeacon "" (-1)
      -- One BidderId.
      , NativeAsset Aftermarket.beaconCurrencySymbol bidderId "" (-1)
      ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case sellerWallet ^. #stakeCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, sellerWallet ^. #stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = bidUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Aftermarket.UnlockUnclaimedAcceptedBid
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

    aftermarketObserverWithdrawal :: TxBodyWithdrawal
    aftermarketObserverWithdrawal = TxBodyWithdrawal
      { stakeAddress = Aftermarket.observerStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Aftermarket.aftermarketObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ 
              Aftermarket.getScriptRef network Aftermarket.aftermarketObserverScriptHash
          , redeemer = toRedeemer 
                     $ Aftermarket.ObserveAftermarket 
                     $ Aftermarket.BeaconId Aftermarket.beaconCurrencySymbol
          , executionBudget = def
          }
      }
