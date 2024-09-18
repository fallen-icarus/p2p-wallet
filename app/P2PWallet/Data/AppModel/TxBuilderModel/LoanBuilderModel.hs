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
  , hasOnlyOneActiveBeaconAction
  , onlyOneActiveBeaconActionError
  , borrowAndLendError
  , acceptAndNegotiateError
  , acceptAndFullPaymentError
  , checkIsSameLoanUserCredential

  , LoanBuilderEvent(..)

  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskUpdate
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.ExpiredClaim
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.InterestApplication
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanKeyBurn
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanPayment
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferAcceptance
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferUpdate
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskCreation
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskUpdate
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.ExpiredClaim
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.InterestApplication
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanKeyBurn
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LoanPayment
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferAcceptance
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferCreation
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferUpdate
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.LoanWallet
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
  -- | Remove the selected offer acceptance from the builder.
  | RemoveSelectedOfferAcceptance Int
  -- | Edit selected offer acceptance. Only the specified collateral can be edited.
  -- Changing the associated ask UTxO to close requires removing the offerAcceptance
  -- and creating a new one from the lending page.
  | EditSelectedOfferAcceptance (AddEvent (Int,OfferAcceptance) (Int,OfferAcceptance))
  -- | Remove the selected loan payment from the builder.
  | RemoveSelectedLoanPayment Int
  -- | Edit selected loan payment.
  | EditSelectedLoanPayment (AddEvent (Int,LoanPayment) (Int,LoanPayment))
  -- | Remove the selected interest application from the builder.
  | RemoveSelectedInterestApplication Int
  -- | Remove the selected expired claim from the builder.
  | RemoveSelectedExpiredClaim Int
  -- | Remove the selected loan key burn from the builder.
  | RemoveSelectedLoanKeyBurn Int
  -- | Remove the selected lender address update from the builder.
  | RemoveSelectedLenderAddressUpdate Int
  -- | Edit selected lender address update.
  | EditSelectedLenderAddressUpdate (AddEvent (Int,LenderAddressUpdate) (Int,LenderAddressUpdate))
  -- | Set the payment to the full amount.
  | SetEditLoanPaymentToFullPayment
  deriving (Show,Eq)

-------------------------------------------------
-- Loan Builder Model
-------------------------------------------------
-- | Due to the number of possible loan actions, they are grouped in this sub-model to
-- help the code stay organized.
data LoanBuilderModel = LoanBuilderModel
  { userCredential :: Maybe Credential
  , askCreations :: [(Int,AskCreation)]
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
  , offerAcceptances :: [(Int,OfferAcceptance)]
  -- | The `Int` is the index into `offerAcceptances`.
  , targetOfferAcceptance :: Maybe (Int,NewOfferAcceptance)
  , loanPayments :: [(Int,LoanPayment)]
  -- | The `Int` is the index into `loanPayments`.
  , targetLoanPayment :: Maybe (Int,NewLoanPayment)
  , interestApplications :: [(Int,InterestApplication)]
  , expiredClaims :: [(Int,ExpiredClaim)]
  , keyBurns :: [(Int,LoanKeyBurn)]
  , addressUpdates :: [(Int,LenderAddressUpdate)]
  , targetAddressUpdate :: Maybe (Int,NewLenderAddressUpdate)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanBuilderModel

instance Default LoanBuilderModel where
  def = LoanBuilderModel
    { userCredential = Nothing
    , askCreations = []
    , targetAskCreation = Nothing
    , askCloses = []
    , askUpdates = []
    , targetAskUpdate = Nothing
    , offerCreations = []
    , targetOfferCreation = Nothing
    , offerCloses = []
    , offerUpdates = []
    , targetOfferUpdate = Nothing
    , offerAcceptances = []
    , targetOfferAcceptance = Nothing
    , loanPayments = []
    , targetLoanPayment = Nothing
    , interestApplications = []
    , expiredClaims = []
    , keyBurns = []
    , addressUpdates = []
    , targetAddressUpdate = Nothing
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
  , null offerAcceptances
  , null loanPayments
  , null interestApplications
  , null expiredClaims
  , null keyBurns
  , null addressUpdates
  , isNothing targetAskCreation
  , isNothing targetAskUpdate
  , isNothing targetOfferCreation
  , isNothing targetOfferUpdate
  , isNothing targetOfferAcceptance
  , isNothing targetLoanPayment
  , isNothing targetAddressUpdate
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

-- | Claim expired collateral (as a lender), unlocking lost collateral (as a borrower), and
-- accepting new offers must occur in separate transactions.
hasOnlyOneActiveBeaconAction :: LoanBuilderModel -> Bool
hasOnlyOneActiveBeaconAction LoanBuilderModel{..}
  | offerAcceptances /= [] = null expiredClaims && null keyBurns
  | expiredClaims /= [] = null offerAcceptances && null keyBurns
  | keyBurns /= [] = null offerAcceptances && null expiredClaims
  | otherwise = True

onlyOneActiveBeaconActionError :: Text
onlyOneActiveBeaconActionError = unwords
  [ "It is not possible to accept an offer, claim collateral, and/or burn excess Key NFTs"
  , "in the same transaction."
  ]

borrowAndLendError :: Text
borrowAndLendError = unwords
  [ "Each transaction must be dedicated to either borrowing or lending."
  , "It is not possible to create/update/close asks and create/update/close offers"
  , "in the same transaction."
  ]

acceptAndNegotiateError :: Text
acceptAndNegotiateError = unwords
  [ "Whenever loan offers are being accepted, all negotiation UTxOs must be"
  , "for the acceptance. This means asks and offers cannot be created/updated/closed"
  , "in the same transaction as offer acceptances."
  ]

acceptAndFullPaymentError :: Text
acceptAndFullPaymentError = unwords
  [ "Loans cannot be accepted in the same transaction where another loan's final payment is made."
  , "This requirement is due to how the on-chain credit history works."
  ]

checkIsSameLoanUserCredential :: Credential -> LoanBuilderModel -> Either Text ()
checkIsSameLoanUserCredential newCredential LoanBuilderModel{userCredential} =
  whenJust userCredential $ \cred ->
    unless (cred == newCredential) $
      Left "All loan actions in a transaction must be for the same user credential."

-------------------------------------------------
-- LoanBuilderModel --> TxBody
-------------------------------------------------
-- Converting the `LoanBuilderModel` to a `TxBody`. This must be done at this level since certain
-- executions depend on what other loan actions are happening in the transaction.
instance AddToTxBody LoanBuilderModel where
  addToTxBody txBody LoanBuilderModel{..} = 
      txBody
        -- Add the ask creations.
        & flip (foldl' addAskCreationToBody) askCreations
        -- Add the ask closes.
        & flip (foldl' addAskCloseToBody) askCloses
        -- Add the ask updates.
        & flip (foldl' addAskUpdateToBody) askUpdates
        -- Add the offer creations.
        & flip (foldl' addOfferCreationToBody) offerCreations
        -- Add the offer closes.
        & flip (foldl' addOfferCloseToBody) offerCloses
        -- Add the offer updates.
        & flip (foldl' addOfferUpdateToBody) offerUpdates
        -- Add the offer acceptances.
        & flip (foldl' addOfferAcceptanceToBody) offerAcceptances
        -- Add the loan payments.
        & flip (foldl' addLoanPaymentToBody) loanPayments
        -- Add the interest applications.
        & flip (foldl' addInterestApplicationToBody) interestApplications
        -- Add the expired claims.
        & flip (foldl' addExpiredClaimToBody) expiredClaims
        -- Add the keys to burn.
        & flip (foldl' addLoanKeyBurnToBody) keyBurns
        -- Add the lender address updates.
        & flip (foldl' addAddressUpdateToBody) addressUpdates
        -- Merge any beacon mints so that there is only one `TxBodyMint` per minting policy.
        & #mints %~ mergeTxBodyMints
        -- Adjust any executions based on whether all mints canceled out.
        & adjustNegotiationExecution
        -- Remove empty mints.
        & #mints %~ removeEmptyMints
        -- Remove duplicate stake withdrawals. This removes duplicate observer entries.
        & #withdrawals %~ ordNubOn (view #stakeCredential)

addAskCreationToBody :: TxBody -> (Int,AskCreation) -> TxBody
addAskCreationToBody txBody (_,AskCreation{..}) =
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
      & #network .~ network
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

addAskCloseToBody :: TxBody -> (Int,AskClose) -> TxBody
addAskCloseToBody txBody (_,AskClose{..}) =
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
      & #network .~ network
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

addAskUpdateToBody :: TxBody -> (Int,AskUpdate) -> TxBody
addAskUpdateToBody txBody (idx,AskUpdate{..}) =
  txBody
    & flip addAskCreationToBody (idx,newAsk)
    & flip addAskCloseToBody (idx,oldAsk)

addOfferCreationToBody :: TxBody -> (Int,OfferCreation) -> TxBody
addOfferCreationToBody txBody (_,OfferCreation{..}) =
    txBody 
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newOutput])
      -- Add the new beacons to be minted. 
      & #mints %~ (newMint:)
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      & #network .~ network
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
      , compoundingInterest = compoundingInterest
      , epochDuration = toPlutusTime . convertDaysToPosixPeriod <$> epochDuration
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

addOfferCloseToBody :: TxBody -> (Int,OfferClose) -> TxBody
addOfferCloseToBody txBody (_,OfferClose{..}) =
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
      & #network .~ network
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

addOfferUpdateToBody :: TxBody -> (Int,OfferUpdate) -> TxBody
addOfferUpdateToBody txBody (idx,OfferUpdate{..}) =
  txBody
    & flip addOfferCreationToBody (idx,newOffer)
    & flip addOfferCloseToBody (idx,oldOffer)

-- | If negotiation beacons do not need to be minted/burned, the beacon script should be executed as
-- a staking script instead.
adjustNegotiationExecution :: TxBody -> TxBody
adjustNegotiationExecution txBody@TxBody{network,mints} = 
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
      { stakeAddress = Loans.negotiationBeaconStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Loans.negotiationBeaconScriptHash
      , stakingScriptInfo = mintInfoToStakingInfo <$> negotiationMint
      }

addOfferAcceptanceToBody :: TxBody -> (Int,OfferAcceptance) -> TxBody
addOfferAcceptanceToBody txBody (_,OfferAcceptance{..}) =
    txBody 
      -- Add the offer and the ask to the inputs.
      & #inputs %~ (<> [newOfferInput, newAskInput])
      -- Add the output and preserve ordering of the output list.
      & #outputs %~ (<> [newCollateralOutput, newLenderOutput])
      -- Add the new beacons to be minted. 
      & #mints %~ (<> [newActiveMint,newNegotiatonBurn])
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      -- Invalid before must be set to the current time. If one is already set, use the maximum of
      -- the two.
      & #invalidBefore %~ updateInvalidBefore (Just lowerBound)
      -- Invalid hereafter must be set to the offer expiration if one is set.
      & #invalidHereafter %~ updateInvalidHereafter upperBound
      & #network .~ network
  where
    lowerBound :: Slot
    lowerBound = posixTimeToSlot slotConfig currentTime

    upperBound :: Maybe Slot
    upperBound = case targetOfferDatum ^. #offerExpiration of
      Nothing -> Nothing
      Just nextDeadline -> Just $
        -- The node can only specify invalidHereafter for 1.5 days into the future due to the
        -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
        -- change the behavior of the protocol.
        min (posixTimeToSlot slotConfig nextDeadline) (129600 + posixTimeToSlot slotConfig currentTime)

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    -- The lender must approve the transaction.
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    lovelaceQuantity :: NativeAsset -> Lovelace
    lovelaceQuantity NativeAsset{policyId,quantity}
      | policyId == "" = Lovelace quantity
      | otherwise = 0

    toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
    toNativeAssetQuantity asset@NativeAsset{policyId}
      | policyId == "" = Nothing
      | otherwise = Just asset

    lenderPayemntAddress :: PaymentAddress
    lenderPayemntAddress = either (const "") fst
                         $ plutusToBech32 network 
                         $ targetOfferDatum ^. #lenderAddress

    targetOfferDatum = fromMaybe def $ loanUTxOOfferDatum offerUTxO

    targetAskDatum = fromMaybe def $ loanUTxOAskDatum askUTxO

    collateralLovelace 
      -- The collateral covers the minUTxOValue
      | deposit - adaCollateral <= 0 = adaCollateral 
      -- Extra ada is needed to cover the minUTxOValue. The value can never be less than
      -- the deposit amount.
      | otherwise = deposit
      where adaCollateral = sum (map lovelaceQuantity collateralAmounts)
    
    lenderPaymentLovelace = Lovelace $ targetOfferDatum ^. #offerDeposit

    collateralNativeAssets = mapMaybe toNativeAssetQuantity collateralAmounts

    activeDatum = Loans.createActiveDatumFromOffer 
      borrowerCredential 
      (offerUTxO ^. #utxoRef)
      currentTime
      targetOfferDatum

    collateralBeacons = 
      -- One Active Beacon.
      [ NativeAsset Loans.activeBeaconCurrencySymbol Loans.activeBeaconName "" 1
      -- One Loan Asset Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (activeDatum ^. #assetBeaconId % #unAssetBeaconId) "" 1
      -- One Loan ID Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (activeDatum ^. #loanId % #unLoanId) "" 1
      -- One Borrower ID Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (activeDatum ^. #borrowerId % #unBorrowerId) "" 1
      ]

    lenderPaymentBeacons = 
      -- One Loan ID Beacon.
      [ NativeAsset Loans.activeBeaconCurrencySymbol (activeDatum ^. #loanId % #unLoanId) "" 1
      ]

    negotiationBeacons =
      -- One Ask Beacon.
      [ NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.askBeaconName "" (-1)
      -- One Loan Asset Beacon from the ask UTxO.
      , NativeAsset 
          Loans.negotiationBeaconCurrencySymbol 
          (targetAskDatum ^. #assetBeaconId % #unAssetBeaconId) 
          "" 
          (-1)
      -- One Offer Beacon.
      , NativeAsset Loans.negotiationBeaconCurrencySymbol Loans.offerBeaconName "" (-1)
      -- One Loan Asset Beacon from the offer UTxO.
      , NativeAsset 
          Loans.negotiationBeaconCurrencySymbol 
          (targetOfferDatum ^. #assetBeaconId % #unAssetBeaconId) 
          "" 
          (-1)
      -- One Lender ID Beacon.
      , NativeAsset 
          Loans.negotiationBeaconCurrencySymbol 
          (targetOfferDatum ^. #lenderId % #unLenderId) 
          "" 
          (-1)
      ]

    newCollateralOutput :: TxBodyOutput
    newCollateralOutput = TxBodyOutput
      { paymentAddress = askUTxO ^. #loanAddress
      , lovelace = collateralLovelace
      , nativeAssets = collateralNativeAssets <> collateralBeacons -- Add the beacons to the output.
      , datum = OutputDatum $ toDatum activeDatum
      }

    newLenderOutput :: TxBodyOutput
    newLenderOutput = TxBodyOutput
      { paymentAddress = lenderPayemntAddress
      , lovelace = lenderPaymentLovelace
      , nativeAssets = lenderPaymentBeacons -- Add the beacons to the output.
      , datum = OutputDatum $ toDatum $ Loans.PaymentDatum
          ( Loans.activeBeaconCurrencySymbol
          , activeDatum ^. #loanId % #unLoanId
          )
      }

    newActiveMint :: TxBodyMint
    newActiveMint = TxBodyMint
      { mintingPolicyHash = Loans.activeBeaconScriptHash
      , nativeAssets = sumNativeAssets $ collateralBeacons <> lenderPaymentBeacons
      , redeemer = toRedeemer $ Loans.CreateActive Loans.negotiationBeaconCurrencySymbol
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.ActiveScript
      , executionBudget = def -- These must be calculated during the build step.
      }

    newNegotiatonBurn :: TxBodyMint
    newNegotiatonBurn = TxBodyMint
      { mintingPolicyHash = Loans.negotiationBeaconScriptHash
      , nativeAssets = negotiationBeacons
      , redeemer = toRedeemer Loans.BurnNegotiationBeacons
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.NegotiationScript
      , executionBudget = def -- These must be calculated during the build step.
      }

    newOfferInput :: TxBodyInput
    newOfferInput = TxBodyInput
      { utxoRef = offerUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Loans.AcceptOffer
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newAskInput :: TxBodyInput
    newAskInput = TxBodyInput
      { utxoRef = askUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer Loans.AcceptOffer
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

addLoanPaymentToBody :: TxBody -> (Int,LoanPayment) -> TxBody
addLoanPaymentToBody txBody (_,LoanPayment{..}) =
    txBody 
      -- Add the collateral input to the input list.
      & #inputs %~ (<> [newCollateralInput])
      -- Add the lender output and preserve ordering of the output list. This output is always
      -- required.
      & #outputs %~ (<> [newLenderOutput])
      -- If a collateral output is required, add the output and preserve ordering of the output list.
      -- This output is only required if this is a partial payment.
      & #outputs %~ maybe id (flip snoc) mCollateralOutput
      -- Add the new beacons to be minted. This burning is only required if it is a full payment.
      & #mints %~ maybe id (flip snoc) mActiveBurn
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      -- Invalid hereafter must be set to the next deadline. Don't overwrite the previously set
      -- invalidHereafter! Instead, use the smaller of the previous setting and the new upperBound.
      & #invalidHereafter %~ updateInvalidHereafter (Just upperBound)
      -- Add the payment observer execution.
      & #withdrawals %~ (paymentObserverStakeWithdrawal:)
      & #network .~ network
  where
    targetActiveDatum@Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum activeUTxO

    upperBound :: Slot
    upperBound = 
      -- The node can only specify invalidHereafter for 1.5 days into the future due to the
      -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
      -- change the behavior of the protocol.
      min (posixTimeToSlot slotConfig nextDeadline) (129600 + posixTimeToSlot slotConfig currentTime)

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    -- The lender must approve the transaction.
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    lovelaceQuantity :: NativeAsset -> Lovelace
    lovelaceQuantity NativeAsset{policyId,quantity}
      | policyId == "" = Lovelace quantity
      | otherwise = 0

    toNativeAssetQuantity :: NativeAsset -> Maybe NativeAsset
    toNativeAssetQuantity asset@NativeAsset{policyId}
      | policyId == "" = Nothing
      | otherwise = Just asset

    nextDeadline = case (+lastEpochBoundary) <$> epochDuration of
      Nothing -> loanExpiration
      Just nextEpochBoundary -> min nextEpochBoundary loanExpiration

    postPaymentDatum = 
      Loans.createPostPaymentActiveDatum (paymentAmount ^. #quantity) targetActiveDatum

    lenderPayemntAddress :: PaymentAddress
    lenderPayemntAddress = either (const "") fst
                         $ plutusToBech32 network 
                         $ targetActiveDatum ^. #lenderAddress

    newCollateralInput :: TxBodyInput
    newCollateralInput = TxBodyInput
      { utxoRef = activeUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer $ Loans.MakePayment $ paymentAmount ^. #quantity
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    collateralBeacons = 
      -- One Active Beacon.
      [ NativeAsset Loans.activeBeaconCurrencySymbol Loans.activeBeaconName "" 1
      -- One Loan Asset Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (targetActiveDatum ^. #assetBeaconId % #unAssetBeaconId) "" 1
      -- One Loan ID Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (targetActiveDatum ^. #loanId % #unLoanId) "" 1
      -- One Borrower ID Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (targetActiveDatum ^. #borrowerId % #unBorrowerId) "" 1
      ]

    collateralLovelace = collateralDeposit + sum (map lovelaceQuantity collateralBalances)
    
    collateralNativeAssets = mapMaybe toNativeAssetQuantity collateralBalances

    mCollateralOutput :: Maybe TxBodyOutput
    mCollateralOutput 
      | isFullPayment = Nothing
      | otherwise = Just $ TxBodyOutput
          { paymentAddress = activeUTxO ^. #loanAddress
          , lovelace = collateralLovelace
          , nativeAssets = collateralNativeAssets <> collateralBeacons -- Add the beacons to the output.
          , datum = OutputDatum $ toDatum postPaymentDatum
          }

    lenderPaymentLovelace = paymentDeposit + lovelaceQuantity paymentAmount

    lenderNativeAssets = mapMaybe toNativeAssetQuantity [paymentAmount]

    newLenderOutput :: TxBodyOutput
    newLenderOutput = TxBodyOutput
      { paymentAddress = lenderPayemntAddress
      , lovelace = lenderPaymentLovelace
      , nativeAssets = lenderNativeAssets
      , datum = OutputDatum $ toDatum $ Loans.PaymentDatum
          ( Loans.activeBeaconCurrencySymbol
          , targetActiveDatum ^. #loanId % #unLoanId
          )
      }

    mActiveBurn :: Maybe TxBodyMint
    mActiveBurn
      | isFullPayment = Just $ TxBodyMint
          { mintingPolicyHash = Loans.activeBeaconScriptHash
          , nativeAssets = map (over #quantity negate) collateralBeacons
          , redeemer = toRedeemer Loans.BurnActiveBeacons
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.ActiveScript
          , executionBudget = def -- These must be calculated during the build step.
          }
      | otherwise = Nothing

    paymentObserverStakeWithdrawal :: TxBodyWithdrawal
    paymentObserverStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Loans.paymentObserverStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Loans.paymentObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.PaymentObserverScript
          , redeemer = toRedeemer Loans.ObservePayment
          , executionBudget = def
          }
      }

addInterestApplicationToBody :: TxBody -> (Int,InterestApplication) -> TxBody
addInterestApplicationToBody txBody (_,InterestApplication{..}) =
    txBody 
      -- Add the collateral input to the input list.
      & #inputs %~ (<> [newCollateralInput])
      -- Add the updated collateral output to the list.
      & #outputs %~ (<> [newCollateralOutput])
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #requiredWitnesses %~ maybe id (:) requiredWitness
      -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
      -- instance of `TxBody`. They will also be sorted.
      & #keyWitnesses %~ maybe id (:) requiredWitness
      -- Invalid hereafter must be set to loan expiration.
      & #invalidHereafter %~ updateInvalidHereafter (Just upperBound)
      -- Add the interest observer execution.
      & #withdrawals %~ (interestObserverStakeWithdrawal:)
      & #network .~ network
  where
    Loans.ActiveDatum{..} = activeDatum

    upperBound :: Slot
    upperBound = 
      -- The node can only specify invalidHereafter for 1.5 days into the future due to the
      -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
      -- change the behavior of the protocol.
      min (posixTimeToSlot slotConfig loanExpiration) (129600 + posixTimeToSlot slotConfig currentTime)

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    -- The lender must approve the transaction.
    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      ScriptCredential _ -> Nothing
      PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)

    postApplicationDatum = 
      Loans.createPostInterestActiveDatum requiredApplicationCount activeDatum

    newCollateralInput :: TxBodyInput
    newCollateralInput = TxBodyInput
      { utxoRef = utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer $ 
              Loans.ApplyInterest (unLovelace extraDeposit) requiredApplicationCount
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newCollateralOutput :: TxBodyOutput
    newCollateralOutput = TxBodyOutput
      { paymentAddress = loanAddress
      , lovelace = lovelace + extraDeposit
      , nativeAssets = nativeAssets
      , datum = OutputDatum $ toDatum postApplicationDatum
      }

    interestObserverStakeWithdrawal :: TxBodyWithdrawal
    interestObserverStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Loans.interestObserverStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Loans.paymentObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = 
              ReferenceWitness $ Loans.getScriptRef network Loans.InterestObserverScript
          , redeemer = toRedeemer Loans.ObserveInterest
          , executionBudget = def
          }
      }

addExpiredClaimToBody :: TxBody -> (Int,ExpiredClaim) -> TxBody
addExpiredClaimToBody txBody (_,ExpiredClaim{..}) =
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
      -- Invalid before must be set to the current time.
      & #invalidBefore %~ updateInvalidBefore (Just lowerBound)
      & #network .~ network
  where
    Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum loanUTxO

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    lowerBound :: Slot
    lowerBound = posixTimeToSlot slotConfig currentTime

    beacons =
      -- One Active Beacon.
      [ NativeAsset Loans.activeBeaconCurrencySymbol Loans.activeBeaconName "" (-1)
      -- One Loan Asset Beacon.
      , NativeAsset Loans.activeBeaconCurrencySymbol (assetBeaconId ^. #unAssetBeaconId) "" (-1)
      -- One Borrower Id.
      , NativeAsset Loans.activeBeaconCurrencySymbol (borrowerId ^. #unBorrowerId) "" (-1)
      -- If the borrower is claiming, then only one loan id must be burned. If the lender
      -- is claiming, then two loan ids must be burned.
      , NativeAsset Loans.activeBeaconCurrencySymbol (loanId ^. #unLoanId) "" $
          if isJust borrowerCredential then -1 else -2
      ]

    requiredWitness :: Maybe KeyWitness
    requiredWitness = case borrowerCredential of
      Nothing -> Nothing
      Just (ScriptCredential _) -> Nothing
      Just (PubKeyCredential pkHash) -> Just $ KeyWitness (pkHash, borrowerStakeKeyDerivation)

    newInput :: TxBodyInput
    newInput = TxBodyInput
      { utxoRef = loanUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer $
              if isJust borrowerCredential 
              then Loans.Unlock
              else Loans.SpendWithKeyNFT
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Loans.activeBeaconCurrencySymbol
      , nativeAssets = beacons
      , redeemer = toRedeemer $ 
          if isJust borrowerCredential 
          then Loans.BurnAndUnlockLost
          else Loans.BurnKeyAndClaimExpired
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.ActiveScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addLoanKeyBurnToBody :: TxBody -> (Int,LoanKeyBurn) -> TxBody
addLoanKeyBurnToBody txBody (_,LoanKeyBurn{..}) =
    txBody
      -- Add the new beacons to be burned. 
      & #mints %~ (newMint:)
      & #network .~ network
  where
    newMint :: TxBodyMint
    newMint = TxBodyMint
      { mintingPolicyHash = policyIdToScriptHash Loans.activeBeaconCurrencySymbol
      , nativeAssets = [loanIdAsset]
      , redeemer = toRedeemer Loans.BurnActiveBeacons
      , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.ActiveScript
      , executionBudget = def -- These must be calculated during the build step.
      }

addAddressUpdateToBody :: TxBody -> (Int,LenderAddressUpdate) -> TxBody
addAddressUpdateToBody txBody (_,LenderAddressUpdate{..}) =
    txBody 
      -- Add the collateral input to the input list.
      & #inputs %~ (<> [newCollateralInput])
      -- Add the updated collateral output to the list.
      & #outputs %~ (<> [newCollateralOutput])
      -- Add the new key output to the list.
      & #outputs %~ (<> [newKeyOutput])
      -- Invalid hereafter must be set to loan expiration.
      & #invalidHereafter %~ updateInvalidHereafter (Just upperBound)
      -- Add the address observer execution.
      & #withdrawals %~ (addressObserverStakeWithdrawal:)
      & #network .~ network
  where
    activeDatum@Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum loanUTxO

    upperBound :: Slot
    upperBound = 
      -- The node can only specify invalidHereafter for 1.5 days into the future due to the
      -- possibility for epoch lengths and slot lengths to change. Using a tighter bound will not
      -- change the behavior of the protocol.
      min (posixTimeToSlot slotConfig loanExpiration) (129600 + posixTimeToSlot slotConfig currentTime)

    slotConfig :: SlotConfig
    slotConfig = case network of
      Mainnet -> mainnetSlotConfig
      Testnet -> testnetSlotConfig

    newPlutusAddress = fromRight (Address (PubKeyCredential "") Nothing) 
                     $ paymentAddressToPlutusAddress newPaymentAddress

    postUpdateDatum = 
      Loans.createPostAddressUpdateActiveDatum newPlutusAddress activeDatum

    newCollateralInput :: TxBodyInput
    newCollateralInput = TxBodyInput
      { utxoRef = loanUTxO ^. #utxoRef
      , spendingScriptInfo = Just $ SpendingScriptInfo
          { datum = InputDatum
          , redeemer = toRedeemer $ 
              Loans.UpdateLenderAddress newPlutusAddress (unLovelace extraDeposit)
          , scriptWitness = ReferenceWitness $ Loans.getScriptRef network Loans.LoanScript
          , executionBudget = def
          , scriptHash = Loans.loanScriptHash
          }
      }

    newCollateralOutput :: TxBodyOutput
    newCollateralOutput = TxBodyOutput
      { paymentAddress = loanUTxO ^. #loanAddress
      , lovelace = loanUTxO ^. #lovelace + extraDeposit
      , nativeAssets = loanUTxO ^. #nativeAssets
      , datum = OutputDatum $ toDatum postUpdateDatum
      }

    newKeyOutput :: TxBodyOutput
    newKeyOutput = TxBodyOutput
      { paymentAddress = newPaymentAddress
      , lovelace = keyDeposit
      , nativeAssets = 
          [ mkNativeAsset Loans.activeBeaconCurrencySymbol (loanId ^. #unLoanId)
              & #quantity .~ 1
          ]
      , datum = OutputDatum $ toDatum $ Loans.PaymentDatum
          ( Loans.activeBeaconCurrencySymbol
          , loanId ^. #unLoanId
          )
      }

    addressObserverStakeWithdrawal :: TxBodyWithdrawal
    addressObserverStakeWithdrawal = TxBodyWithdrawal
      { stakeAddress = Loans.addressUpdateObserverStakeAddress network
      , lovelace = 0
      , stakeCredential = ScriptCredential Loans.addressUpdateObserverScriptHash
      , stakingScriptInfo = Just $ StakingScriptInfo
          { scriptWitness = 
              ReferenceWitness $ Loans.getScriptRef network Loans.AddressObserverScript
          , redeemer = toRedeemer Loans.ObserveAddressUpdate
          , executionBudget = def
          }
      }
