{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel
  ( LoanBuilderModel(..)
  , emptyLoanBuilderModel
  , isEmptyLoanBuilderModel
  , hasAskActions
  , hasOfferActions
  , borrowAndLendError

  , LoanBuilderEvent(..)

  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskUpdate
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferUpdate
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskUpdate
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Builder Events
-------------------------------------------------
-- | All events dealing with loans. This is separate to help stay organized.
data LoanBuilderEvent
  -- | Remove the selected ask creation from the builder.
  = RemoveSelectedAskCreation Int
  -- | Edit selected ask creation.
  | EditSelectedAskCreation (AddEvent (Int,AskCreation) (Int,AskCreation))
  -- | Change the desired number of ask creations with these conditions. The first int is the index
  -- into the ask creations list and the second is the new count.
  | ChangeAskCreationCount Int Int
  -- | Remove the selected ask close from the builder.
  | RemoveSelectedAskClose Int
  -- | Remove the selected ask update from the builder.
  | RemoveSelectedAskUpdate Int
  -- | Edit selected ask update.
  | EditSelectedAskUpdate (AddEvent (Int,AskUpdate) (Int,AskCreation))
  -- | Remove the selected offer creation from the builder.
  | RemoveSelectedOfferCreation Int
  -- | Edit selected offer creation.
  | EditSelectedOfferCreation (AddEvent (Int,OfferCreation) (Int,OfferCreation))
  -- | Remove the selected offer close from the builder.
  | RemoveSelectedOfferClose Int
  -- | Remove the selected offer update from the builder.
  | RemoveSelectedOfferUpdate Int
  -- | Edit selected offer update.
  | EditSelectedOfferUpdate (AddEvent (Int,OfferUpdate) (Int,OfferCreation))
  deriving (Show,Eq)

-------------------------------------------------
-- Loan Builder Model
-------------------------------------------------
-- | Due to the number of possible loan actions, they are grouped in this sub-model to
-- help the code stay organized.
data LoanBuilderModel = LoanBuilderModel
  { askCreations :: [(Int,AskCreation)]
  , targetAskCreation :: Maybe (Int,NewAskCreation)
  , askCloses :: [(Int,AskClose)]
  , askUpdates :: [(Int,AskUpdate)]
  -- | The `Int` is the index into `askUpdates`.
  , targetAskUpdate :: Maybe (Int,NewAskCreation)
  , offerCreations :: [(Int,OfferCreation)]
  , targetOfferCreation :: Maybe (Int,NewOfferCreation)
  , offerCloses :: [(Int,OfferClose)]
  , offerUpdates :: [(Int,OfferUpdate)]
  -- | The `Int` is the index into `offerUpdates`.
  , targetOfferUpdate :: Maybe (Int,NewOfferCreation)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanBuilderModel

instance Default LoanBuilderModel where
  def = LoanBuilderModel
    { askCreations = []
    , targetAskCreation = Nothing
    , askCloses = []
    , askUpdates = []
    , targetAskUpdate = Nothing
    , offerCreations = []
    , targetOfferCreation = Nothing
    , offerCloses = []
    , offerUpdates = []
    , targetOfferUpdate = Nothing
    }

-- | This is just an alias for `def`. The name is more clear what it is.
emptyLoanBuilderModel :: LoanBuilderModel
emptyLoanBuilderModel = def

isEmptyLoanBuilderModel :: LoanBuilderModel -> Bool
isEmptyLoanBuilderModel LoanBuilderModel{..} = and
  [ null askCreations
  , null askCloses
  , null askUpdates
  , null offerCreations
  , null offerCloses
  , null offerUpdates
  , isNothing targetAskCreation
  , isNothing targetAskUpdate
  , isNothing targetOfferCreation
  , isNothing targetOfferUpdate
  ]

hasAskActions :: LoanBuilderModel -> Bool
hasAskActions LoanBuilderModel{..} = or
  [ askCreations /= []
  , askCloses /= []
  , askUpdates /= []
  ]

hasOfferActions :: LoanBuilderModel -> Bool
hasOfferActions LoanBuilderModel{..} = or
  [ offerCreations /= []
  , offerCloses /= []
  , offerUpdates /= []
  ]

borrowAndLendError :: AppError
borrowAndLendError = AppError $ unwords
  [ "Each transaction must be dedicated to either borrowing or lending."
  , "It is not possible to create/update/close asks and create/update/close offers"
  , "in the same transaction."
  ]

-------------------------------------------------
-- LoanBuilderModel --> TxBody
-------------------------------------------------
-- Converting the `LoanBuilderModel` to a `TxBody`. This must be done at this level since certain
-- executions depend on what other loan actions are happening in the transaction.
instance AddToTxBody LoanBuilderModel where
  addToTxBody txBody LoanBuilderModel{..} = 
      txBody
        -- Add the ask creations.
        & flip (foldl' addAskCreationToBuilder) askCreations
        -- Add the ask closes.
        & flip (foldl' addAskCloseToBuilder) askCloses
        -- Add the ask updates.
        & flip (foldl' addAskUpdateToBuilder) askUpdates
        -- Add the offer creations.
        & flip (foldl' addOfferCreationToBuilder) offerCreations
        -- Add the offer closes.
        & flip (foldl' addOfferCloseToBuilder) offerCloses
        -- Add the offer updates.
        & flip (foldl' addOfferUpdateToBuilder) offerUpdates
        -- Merge any beacon mints so that there is only one `TxBodyMint` per minting policy.
        & #mints %~ mergeTxBodyMints
        -- Adjust any executions based on whether all mints canceled out.
        & adjustNegotiationExecution
        -- Remove empty mints.
        & #mints %~ removeEmptyMints

addAskCreationToBuilder :: TxBody -> (Int,AskCreation) -> TxBody
addAskCreationToBuilder txBody (_,AskCreation{..}) =
    txBody 
      -- Add one instance of the output per count and preserve ordering of the
      -- output list.
      & #outputs %~ (<> replicate count newOutput)
      -- Add the new beacons to be minted. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
  where
    -- The borrower must approve the transaction.
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, borrowerKeyDerivation)

    toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
    toNativeAssetQuantity asset@NativeAsset{policyId}
      -- Ada is not a native asset and is already part of the deposit.
      | policyId == "" = Nothing
      -- One unit of each asset must be deposited.
      | otherwise = Just $ asset & #quantity .~ 1

    -- Get the total amount assets required for the output. This does not have the beacons
    -- yet.
    (lovelace, nativeAssets) = 
      ( deposit
      , mapMaybe toNativeAssetQuantity collateral
      )

    -- Convert the number of days to total milliseconds.
    loanDuration :: PlutusTime
    loanDuration = toPlutusTime $ convertDaysToPosixPeriod loanTerm

    askDatum :: Loans.AskDatum 
    askDatum = Loans.createAskDatum borrowerCredential loanAmount loanDuration collateral

    beacons = 
      -- One Ask Beacon.
      [ NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.askBeaconName "" 1
      -- One Loan Asset Beacon.
      , NativeAsset Loans.negotiationBeaconCurrencySymbol (askDatum ^. #assetBeaconId % #unAssetBeaconId) "" 1
      ]

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = paymentAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets <> beacons -- Add the beacons to the output.
      , datum = OutputDatum $ toDatum askDatum
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = Loans.negotiationBeaconScriptHash
      , nativeAssets = 
          -- Increase the quantity by the total number of required outputs.
          sumNativeAssets $ concat $ replicate count beacons
      , redeemer = toRedeemer $ Loans.CreateCloseOrUpdateAsk borrowerCredential
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.NegotiationScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addAskCloseToBuilder :: TxBody -> (Int,AskClose) -> TxBody
addAskCloseToBuilder txBody (_,AskClose{..}) =
    txBody
      -- Add the new ask to the inputs.
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
    beacons = case askDatum of
      Nothing -> []
      Just Loans.AskDatum{assetBeaconId} ->
        -- One Ask Beacon.
        [ NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.askBeaconName "" (-1)
        -- One Loan Asset Beacon.
        , NativeAsset Loans.negotiationBeaconCurrencySymbol (assetBeaconId ^. #unAssetBeaconId) "" (-1)
        ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Loans.CloseOrUpdateAsk
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Loans.negotiationBeaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = toRedeemer $ Loans.CreateCloseOrUpdateAsk borrowerCredential
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.NegotiationScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addAskUpdateToBuilder :: TxBody -> (Int,AskUpdate) -> TxBody
addAskUpdateToBuilder txBody (idx,AskUpdate{..}) =
  txBody
    & flip addAskCreationToBuilder (idx,newAsk)
    & flip addAskCloseToBuilder (idx,oldAsk)

addOfferCreationToBuilder :: TxBody -> (Int,OfferCreation) -> TxBody
addOfferCreationToBuilder txBody (_,OfferCreation{..}) =
    txBody 
      -- Add one instance of the output per count and preserve ordering of the
      -- output list.
      & #outputs %~ (<> [newOutput])
      -- Add the new beacons to be minted. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
  where
    -- The lender must approve the transaction.
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case lenderCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, lenderKeyDerivation)

    lovelace = case loanAmount ^. #policyId of
      "" -> deposit + Lovelace (loanAmount ^. #quantity)
      _ -> deposit

    nativeAssets = case loanAmount ^. #policyId of
      "" -> []
      _ -> [loanAmount]

    lenderTerms :: Loans.LenderTerms
    lenderTerms = Loans.LenderTerms
      { lenderCredential = lenderCredential
      , lenderAddress = paymentWallet ^. #paymentAddress
      , loanTerm = toPlutusTime $ convertDaysToPosixPeriod loanTerm
      , loanAmount = loanAmount
      , interest = interest
      , compoundFrequency = toPlutusTime . convertDaysToPosixPeriod <$> compoundFrequency
      , minPayment = minPayment
      , penalty = penalty
      , claimPeriod = toPlutusTime $ convertDaysToPosixPeriod claimPeriod
      , offerExpiration = (+ currentTime) . toPlutusTime . convertDaysToPosixPeriod <$> offerExpiration
      , collateralization = sortOn fst collateralization
      , offerDeposit = unLovelace deposit
      , collateralIsSwappable = collateralIsSwappable
      }

    offerDatum = Loans.createOfferDatum lenderTerms

    beacons = 
      -- One Offer Beacon.
      [ NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.offerBeaconName "" 1
      -- One Loan Asset Beacon.
      , NativeAsset Loans.negotiationBeaconCurrencySymbol (offerDatum ^. #assetBeaconId % #unAssetBeaconId) "" 1
      -- One Lender ID Beacon.
      , NativeAsset Loans.negotiationBeaconCurrencySymbol (offerDatum ^. #lenderId % #unLenderId) "" 1
      ]

    newOutput :: TxBodyOutput
    newOutput = TxBodyOutput
      { paymentAddress = loanAddress
      , lovelace = lovelace
      , nativeAssets = nativeAssets <> beacons -- Add the beacons to the output.
      , datum = OutputDatum $ toDatum offerDatum
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = Loans.negotiationBeaconScriptHash
      , nativeAssets = beacons
      , redeemer = toRedeemer $ Loans.CreateCloseOrUpdateOffer lenderCredential
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.NegotiationScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addOfferCloseToBuilder :: TxBody -> (Int,OfferClose) -> TxBody
addOfferCloseToBuilder txBody (_,OfferClose{..}) =
    txBody
      -- Add the new offer to the inputs.
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
    beacons = case offerDatum of
      Nothing -> []
      Just Loans.OfferDatum{lenderId,assetBeaconId} ->
        -- One Ask Beacon.
        [ NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.offerBeaconName "" (-1)
        -- One Loan Asset Beacon.
        , NativeAsset Loans.negotiationBeaconCurrencySymbol (assetBeaconId ^. #unAssetBeaconId) "" (-1)
        -- One Lender Id.
        , NativeAsset Loans.negotiationBeaconCurrencySymbol (lenderId ^. #unLenderId) "" (-1)
        ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case lenderCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Loans.CloseOrUpdateOffer
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Loans.negotiationBeaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = toRedeemer $ Loans.CreateCloseOrUpdateOffer lenderCredential
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.NegotiationScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addOfferUpdateToBuilder :: TxBody -> (Int,OfferUpdate) -> TxBody
addOfferUpdateToBuilder txBody (idx,OfferUpdate{..}) =
  txBody
    & flip addOfferCreationToBuilder (idx,newOffer)
    & flip addOfferCloseToBuilder (idx,oldOffer)

-- | If negotiation beacons do not need to be minted/burned, the beacon script should be executed as
-- a staking script instead.
adjustNegotiationExecution :: TxBody -> TxBody
adjustNegotiationExecution txBody@TxBody{mints} = 
    txBody
      & adjustExecution
  where
    adjustExecution :: TxBody -> TxBody
    adjustExecution body
      | (isAskRedeemer || isOfferRedeemer) && beacons == Just [] =
          body & #withdrawals %~ (negotiationStakeWithdrawal:)
      | otherwise = body

    negotiationMint :: Maybe TxBodyMint
    negotiationMint = find ((== Loans.negotiationBeaconScriptHash) . view #mintingPolicyHash) mints

    beacons :: Maybe [NativeAsset]
    beacons = view #nativeAssets <$> negotiationMint

    negotiationRedeemer :: Maybe Loans.NegotiationBeaconsRedeemer
    negotiationRedeemer = 
      fmap (view #redeemer) negotiationMint >>= fromRedeemer @Loans.NegotiationBeaconsRedeemer

    isAskRedeemer :: Bool
    isAskRedeemer = maybe False (is Loans._CreateCloseOrUpdateAsk) negotiationRedeemer

    isOfferRedeemer :: Bool
    isOfferRedeemer = maybe False (is Loans._CreateCloseOrUpdateOffer) negotiationRedeemer

    mintInfoToStakingInfo :: TxBodyMint -> StakingScriptInfo
    mintInfoToStakingInfo TxBodyMint{..} = StakingScriptInfo
      { scriptWitness = scriptWitness
      , redeemer = redeemer
      , executionBudget = executionBudget
      }

    negotiationStakeWithdrawal :: TxBodyWithdrawal
    negotiationStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Loans.negotiationBeaconStakeAddress Testnet
      , lovelace = 0
      , stakeCredential = ScriptCredential Loans.negotiationBeaconScriptHash
      , stakingScriptInfo = mintInfoToStakingInfo <$> negotiationMint
      }
