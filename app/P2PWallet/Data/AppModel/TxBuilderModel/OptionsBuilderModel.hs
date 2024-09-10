{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel
  ( OptionsBuilderModel(..)
  , emptyOptionsBuilderModel
  , isEmptyOptionsBuilderModel

  , OptionsBuilderEvent(..)

  , module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalPurchase
  , module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalUpdate
  ) where

import Data.List (minimum,(!!))

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalClose
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalCreation
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalPurchase
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Options Builder Events
-------------------------------------------------
-- | All events dealing with Options. This is separate to help stay organized.
data OptionsBuilderEvent
  -- | Remove the selected proposal creation from the builder.
  = RemoveSelectedProposalCreation Int
  -- | Edit selected proposal creation.
  | EditSelectedProposalCreation (AddEvent (Int,ProposalCreation) (Int,ProposalCreation))
  -- | Change the desired number of proposalCreations creations with these conditions. The first 
  -- int is the index into the proposalCreations creations list and the second is the new count.
  | ChangeProposalCreationCount Int Int
  -- | Remove the selected proposal close from the builder.
  | RemoveSelectedProposalClose Int
  -- | Remove the selected proposal update from the builder.
  | RemoveSelectedProposalUpdate Int
  -- | Edit selected proposal update.
  | EditSelectedProposalUpdate (AddEvent (Int,ProposalUpdate) (Int,ProposalCreation))
  -- | Remove the selected proposal purchase from the builder.
  | RemoveSelectedProposalPurchase Int
  -- | Edit selected proposal purchase. This can only change the selected terms. The `Int` is
  -- the index into `proposalPurchases` and the `Integer` is the new desired terms index.
  | EditSelectedProposalPurchase (ProcessEvent (Int,ProposalPurchase) (Int,ProposalPurchase))
  deriving (Show,Eq)

-------------------------------------------------
-- Options Builder Model
-------------------------------------------------
-- | Due to the number of possible options actions, they are grouped in this sub-model to
-- help the code stay organized.
data OptionsBuilderModel = OptionsBuilderModel
  { proposalCreations :: [(Int,ProposalCreation)]
  , targetProposalCreation :: Maybe (Int,NewProposalCreation)
  , proposalCloses :: [(Int,ProposalClose)]
  , proposalUpdates :: [(Int,ProposalUpdate)]
  -- | The `Int` is the index into `proposalUpdates`.
  , targetProposalUpdate :: Maybe (Int,NewProposalCreation)
  , proposalPurchases :: [(Int,ProposalPurchase)]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsBuilderModel

instance Default OptionsBuilderModel where
  def = OptionsBuilderModel
    { proposalCreations = []
    , targetProposalCreation = Nothing
    , proposalCloses = []
    , proposalUpdates = []
    , targetProposalUpdate = Nothing
    , proposalPurchases = []
    }

-- | This is just an alias for `def`. The name is more clear what it is.
emptyOptionsBuilderModel :: OptionsBuilderModel
emptyOptionsBuilderModel = def

isEmptyOptionsBuilderModel :: OptionsBuilderModel -> Bool
isEmptyOptionsBuilderModel OptionsBuilderModel{..} = and
  [ null proposalCreations
  , null proposalCloses
  , null proposalUpdates
  , null proposalPurchases
  , isNothing targetProposalCreation
  , isNothing targetProposalUpdate
  ]

-------------------------------------------------
-- OptionsBuilderModel --> TxBody
-------------------------------------------------
-- Converting the `OptionsBuilderModel` to a `TxBody`. 
instance AddToTxBody OptionsBuilderModel where
  addToTxBody txBody OptionsBuilderModel{..} = 
      txBody
        -- Add the proposal creations.
        & flip (foldl' addProposalCreationToBody) proposalCreations
        -- Add the proposal closes.
        & flip (foldl' addProposalCloseToBody) proposalCloses
        -- Add the proposal updates.
        & flip (foldl' addProposalUpdateToBody) proposalUpdates
        -- Add the proposal purchases.
        & flip (foldl' addProposalPurchaseToBody) proposalPurchases
        -- Merge any beacon mints so that there is only one `TxBodyMint` per minting policy.
        & #mints %~ mergeTxBodyMints
        -- Adjust any executions based on whether all mints canceled out.
        & adjustProposalExecution
        -- Remove empty mints.
        & #mints %~ removeEmptyMints
        -- Remove duplicate stake withdrawals. This removes duplicate observer entries.
        & #withdrawals %~ ordNubOn (view #stakeCredential)

addProposalCreationToBody :: TxBody -> (Int,ProposalCreation) -> TxBody
addProposalCreationToBody txBody (_,ProposalCreation{..}) =
    txBody 
      -- Add one instance of the output per count and preserve ordering of the output list.
      & #outputs %~ (<> replicate count newOutput)
      -- Add the new beacons to be minted. 
      & #mints %~ (newMint:)
      -- Invalid hereafter must be set to the closest expiration among the possible terms.
      & #invalidHereafter ?~ upperBound
      & #network .~ network
  where
    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    -- The node can only specify invalidHereafter for 1.5 days into the future due to the
    -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
    -- change the behavior of the protocol.
    upperBound :: Slot
    upperBound = 
      let expirations = map (posixTimeToSlot slotConfig . toPlutusTime . view _3) possibleTerms
       in minimum (129600 + posixTimeToSlot slotConfig currentTime : expirations)

    lovelace = case offerAsset ^. #policyId of
      "" -> deposit + Lovelace (offerAsset ^. #quantity)
      _ -> deposit

    nativeAssets = case offerAsset ^. #policyId of
      "" -> []
      _ -> [offerAsset]

    newProposalInfo :: Options.NewProposalInfo
    newProposalInfo = Options.NewProposalInfo
      { offerAsset = fromNativeAsset offerAsset
      , offerQuantity = offerAsset ^. #quantity
      , askAsset = fromNativeAsset askAsset
      , premiumAsset = fromNativeAsset premiumAsset
      , contractDeposit = unLovelace deposit
      , paymentAddress = fromRight (Address (PubKeyCredential "") Nothing) 
                       $ paymentAddressToPlutusAddress
                       $ paymentWallet ^. #paymentAddress
      , possibleTerms = for possibleTerms $ \(premium,strikePrice,expiration) -> Options.Terms
          { premium = premium ^. #quantity
          , strikePrice = fromRational strikePrice
          , expiration = toPlutusTime expiration
          }
      }

    proposalDatum :: Options.ProposalDatum 
    proposalDatum@Options.ProposalDatum{askBeacon,offerBeacon,premiumBeacon,tradingPairBeacon} = 
      Options.createProposalDatum newProposalInfo

    beacons = 
      -- One Ask Beacon.
      [ NativeAsset Options.proposalBeaconCurrencySymbol (askBeacon ^. #unAskBeacon) "" 1
      -- One Offer Beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (offerBeacon ^. #unOfferBeacon) "" 1
      -- One premium beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (premiumBeacon ^. #unPremiumBeacon) "" 1
      -- One tradingPair beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (tradingPairBeacon ^. #unTradingPairBeacon) "" 1
      ]

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = optionsAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets <> beacons -- Add the beacons to the output.
      , datum = OutputDatum $ toDatum proposalDatum
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = Options.proposalBeaconScriptHash
      , nativeAssets = 
          -- Increase the quantity by the total number of required outputs.
          sumNativeAssets $ concat $ replicate count beacons
      , redeemer = toRedeemer Options.CreateCloseOrUpdateProposals
      , scriptWitness = ReferenceWitness $ Options.getScriptRef network Options.proposalBeaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

-- | If proposal beacons do not need to be minted/burned, the beacon script should be executed as
-- a staking script instead.
adjustProposalExecution :: TxBody -> TxBody
adjustProposalExecution txBody@TxBody{network,mints} = 
    txBody
      & adjustExecution
  where
    adjustExecution :: TxBody -> TxBody
    adjustExecution body
      | isProposalRedeemer && beacons == Just [] =
          body & #withdrawals %~ (proposalStakeWithdrawal:)
      | otherwise = body

    proposalMint :: Maybe TxBodyMint
    proposalMint = find ((== Options.proposalBeaconScriptHash) . view #mintingPolicyHash) mints

    beacons :: Maybe [NativeAsset]
    beacons = view #nativeAssets <$> proposalMint

    proposalRedeemer :: Maybe Options.ProposalBeaconsRedeemer
    proposalRedeemer = 
      fmap (view #redeemer) proposalMint >>= fromRedeemer @Options.ProposalBeaconsRedeemer

    isProposalRedeemer :: Bool
    isProposalRedeemer = maybe False (is Options._CreateCloseOrUpdateProposals) proposalRedeemer

    mintInfoToStakingInfo :: TxBodyMint -> StakingScriptInfo
    mintInfoToStakingInfo TxBodyMint{..} = StakingScriptInfo
      { scriptWitness = scriptWitness
      , redeemer = redeemer
      , executionBudget = executionBudget
      }

    proposalStakeWithdrawal :: TxBodyWithdrawal
    proposalStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Options.proposalBeaconStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Options.proposalBeaconScriptHash
      , stakingScriptInfo = mintInfoToStakingInfo <$> proposalMint
      }

addProposalCloseToBody :: TxBody -> (Int,ProposalClose) -> TxBody
addProposalCloseToBody txBody (_,ProposalClose{..}) =
    txBody
      -- Add the new proposal to the inputs.
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
    beacons = case proposalDatum of
      Nothing -> []
      Just Options.ProposalDatum{offerBeacon,askBeacon,premiumBeacon,tradingPairBeacon} ->
        -- One Ask Beacon.
        [ NativeAsset Options.proposalBeaconCurrencySymbol (askBeacon ^. #unAskBeacon) "" (-1)
        -- One Offer Beacon.
        , NativeAsset Options.proposalBeaconCurrencySymbol (offerBeacon ^. #unOfferBeacon) "" (-1)
        -- One premium beacon.
        , NativeAsset Options.proposalBeaconCurrencySymbol (premiumBeacon ^. #unPremiumBeacon) "" (-1)
        -- One tradingPair beacon.
        , NativeAsset Options.proposalBeaconCurrencySymbol (tradingPairBeacon ^. #unTradingPairBeacon) "" (-1)
        ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case writerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Options.CloseOrUpdateProposal
          , scriptWitness = ReferenceWitness $ Options.getScriptRef network Options.optionsScriptHash
          , executionBudget = def
          , scriptHash = Options.optionsScriptHash
          }
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Options.proposalBeaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = toRedeemer Options.CreateCloseOrUpdateProposals
      , scriptWitness = ReferenceWitness $ Options.getScriptRef network Options.proposalBeaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

addProposalUpdateToBody :: TxBody -> (Int,ProposalUpdate) -> TxBody
addProposalUpdateToBody txBody (idx,ProposalUpdate{..}) =
  txBody
    & flip addProposalCreationToBody (idx,newProposal)
    & flip addProposalCloseToBody (idx,oldProposal)

addProposalPurchaseToBody :: TxBody -> (Int,ProposalPurchase) -> TxBody
addProposalPurchaseToBody txBody (_,ProposalPurchase{..}) = 
    txBody
      -- Add the proposal input to the list of inputs.
      & #inputs %~ (<> [newProposalInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newPremiumOutput, newContractOutput])
      -- Add the new beacons to be minted. 
      & #mints %~ (<> [newActiveMint,newProposalBurn])
      & #network .~ network

  where
    OptionsUTxO{..} = proposalUTxO
    proposalDatum@Options.ProposalDatum{..} = 
      fromMaybe def $ optionsUTxOProposalDatum proposalUTxO
    Options.Terms{premium} = possibleTerms !! fromIntegral desiredTerms
    activeDatum = Options.createActiveDatumFromProposal desiredTerms utxoRef proposalDatum
    premiumAsNativeAsset = toNativeAsset premiumAsset & #quantity .~ premium

    writerPayemntAddress :: PaymentAddress
    writerPayemntAddress = either (const "") fst
                         $ plutusToBech32 network paymentAddress

    premiumLovelace
      | premiumAsNativeAsset ^. #policyId == "" = Lovelace premium + premiumDeposit
      | otherwise = premiumDeposit

    premiumNativeAssets
      | premiumAsNativeAsset ^. #policyId == "" = []
      | otherwise = [premiumAsNativeAsset]

    contractNativeAssets = 
      filter ((/= Options.proposalBeaconCurrencySymbol) . view #policyId) nativeAssets <> 
        contractUTxOBeacons

    proposalBeaconBurns =
      -- One Ask Beacon.
      [ NativeAsset Options.proposalBeaconCurrencySymbol (askBeacon ^. #unAskBeacon) "" (-1)
      -- One Offer Beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (offerBeacon ^. #unOfferBeacon) "" (-1)
      -- One premium beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (premiumBeacon ^. #unPremiumBeacon) "" (-1)
      -- One tradingPair beacon.
      , NativeAsset Options.proposalBeaconCurrencySymbol (tradingPairBeacon ^. #unTradingPairBeacon) "" (-1)
      ]

    contractUTxOBeacons =
      -- One Ask Beacon.
      [ NativeAsset Options.activeBeaconCurrencySymbol (askBeacon ^. #unAskBeacon) "" 1
      -- One Offer Beacon.
      , NativeAsset Options.activeBeaconCurrencySymbol (offerBeacon ^. #unOfferBeacon) "" 1
      -- One Contract IDs beacon.
      , NativeAsset Options.activeBeaconCurrencySymbol (activeDatum ^. #contractId % #unContractId) "" 1
      -- One tradingPair beacon.
      , NativeAsset Options.activeBeaconCurrencySymbol (tradingPairBeacon ^. #unTradingPairBeacon) "" 1
      ]

    -- Must mint one more contract id in addition to what gets stored with the contract UTxO.
    activeBeaconMints = sumNativeAssets $ (:contractUTxOBeacons) $
      NativeAsset Options.activeBeaconCurrencySymbol (activeDatum ^. #contractId % #unContractId) "" 1

    newProposalInput :: TxBodyInput
    newProposalInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer $ Options.PurchaseContract desiredTerms
          , scriptWitness = 
              ReferenceWitness $ Options.getScriptRef network Options.optionsScriptHash
          , executionBudget = def
          , scriptHash = Options.optionsScriptHash
          }
      }

    newPremiumOutput :: TxBodyOutput
    newPremiumOutput = TxBodyOutput
      { paymentAddress = writerPayemntAddress
      , lovelace = premiumLovelace
      , nativeAssets = premiumNativeAssets
      , datum = OutputDatum $ toDatum $ Options.PaymentDatum
          ( Options.ActiveBeaconId $ Options.activeBeaconCurrencySymbol
          , activeDatum ^. #contractId
          )
      }

    newContractOutput :: TxBodyOutput
    newContractOutput = TxBodyOutput
      { paymentAddress = optionsAddress
      , lovelace = lovelace + extraContractDeposit
      , nativeAssets = contractNativeAssets
      , datum = OutputDatum $ toDatum activeDatum
      }

    newProposalBurn :: TxBodyMint
    newProposalBurn = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Options.proposalBeaconCurrencySymbol
      , nativeAssets = proposalBeaconBurns
      , redeemer = 
          -- BurnProposalBeacons would be cheaper, but this redeemer is easier to work with when
          -- creating composed transactions.
          toRedeemer Options.CreateCloseOrUpdateProposals
      , scriptWitness = ReferenceWitness $ Options.getScriptRef network Options.proposalBeaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }

    newActiveMint :: TxBodyMint
    newActiveMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Options.activeBeaconCurrencySymbol
      , nativeAssets = activeBeaconMints
      , redeemer = toRedeemer 
                 $ Options.PurchaseExecuteOrCloseExpiredContracts Options.proposalBeaconCurrencySymbol
      , scriptWitness = ReferenceWitness $ Options.getScriptRef network Options.activeBeaconScriptHash
      , executionBudget = def -- These must be calculated during the build step.
      }
