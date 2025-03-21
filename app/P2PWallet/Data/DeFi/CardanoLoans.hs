{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoLoans
  ( -- * Datums
    AskDatum(..)
  , OfferDatum(..)
  , ActiveDatum(..)
  , PaymentDatum(..)

    -- * Redeemers
  , LoanRedeemer(..)
  , PaymentObserverRedeemer(..)
  , InterestObserverRedeemer(..)
  , AddressUpdateObserverRedeemer(..)
  , NegotiationBeaconsRedeemer(..)
  , ActiveBeaconsRedeemer(..)
  , _CreateCloseOrUpdateAsk
  , _CreateCloseOrUpdateOffer

    -- * Smart Contracts
  , proxyScript
  , proxyScriptSize
  , proxyScriptHash
  , loanScript
  , loanScriptSize
  , loanScriptHash
  , paymentObserverScript
  , paymentObserverScriptSize
  , paymentObserverScriptHash
  , interestObserverScript
  , interestObserverScriptSize
  , interestObserverScriptHash
  , addressUpdateObserverScript
  , addressUpdateObserverScriptSize
  , addressUpdateObserverScriptHash
  , activeBeaconScript
  , activeBeaconScriptSize
  , activeBeaconScriptHash
  , activeBeaconCurrencySymbol
  , negotiationBeaconScript
  , negotiationBeaconScriptSize
  , negotiationBeaconScriptHash
  , negotiationBeaconCurrencySymbol
  
    -- * Beacon Names
  , genLoanAssetBeaconName
  , genLoanId
  , genLenderId
  , genBorrowerId
  , askBeaconName
  , offerBeaconName
  , activeBeaconName
    
    -- * Calculations
  , applyInterest
  , applyInterestNTimes
  , subtractPayment

    -- * Creating Datums
  , createAskDatum
  , LenderTerms(..)
  , createOfferDatum
  , createActiveDatumFromOffer
  , createPostPaymentActiveDatum
  , createPostInterestActiveDatum
  , createPostAddressUpdateActiveDatum

    -- * Protocol Addresses
  , isValidLoanPaymentAddress
  , genLoanAddress
  , isLoanAddress
  , genProxyAddress
  , isProxyAddress
  , negotiationBeaconStakeAddress
  , paymentObserverStakeAddress
  , interestObserverStakeAddress
  , addressUpdateObserverStakeAddress

    -- * Reference Scripts Output References
  , LoanScriptType(..)
  , getScriptRef
  , proxyScriptTestnetRef

    -- * Re-exports
  , module P2PWallet.Data.DeFi.CardanoLoans.Internal
  ) where

import qualified Data.Map.Strict as Map
import Data.Aeson

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx
import qualified PlutusLedgerApi.V2 as PV2

import CardanoLoans.Blueprints

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.DeFi.CardanoLoans.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Ask Datum
-------------------------------------------------
data AskDatum = AskDatum
  -- | The policy id for the negotiation beacon script.
  { negotiationBeaconId :: NegotiationBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The borrower's staking credential as a token name.
  , borrowerId :: BorrowerId
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeaconId :: AssetBeaconId
  -- | The size of the loan.
  , loanPrincipal :: Integer
  -- | How long the loan is active once accepted.
  , loanTerm :: PlutusTime
  -- | The assets that will be used as collateral
  , collateral :: Collateral
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AskDatum

instance Default AskDatum where
  def = AskDatum
    { negotiationBeaconId = ""
    , activeBeaconId = ""
    , borrowerId = ""
    , loanAsset = Asset ("","")
    , assetBeaconId = ""
    , loanPrincipal = 0
    , loanTerm = 0
    , collateral = Collateral []
    }

instance ToJSON AskDatum where
  toJSON AskDatum{..} =
    object [ "negotiation_beacon_id" .= negotiationBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "borrower_id" .= borrowerId
           , "loan_asset" .= loanAsset
           , "asset_beacon_id" .= assetBeaconId
           , "principal" .= loanPrincipal
           , "loan_term" .= loanTerm
           , "collateral" .= collateral
           ]

instance FromJSON AskDatum where
  parseJSON = withObject "AskDatum" $ \o ->
    AskDatum
      <$> (o .: "negotiation_beacon_id")
      <*> (o .: "active_beacon_id")
      <*> (o .: "borrower_id")
      <*> (o .: "loan_asset")
      <*> (o .: "asset_beacon_id")
      <*> (o .: "principal")
      <*> (o .: "loan_term")
      <*> (o .: "collateral")

-------------------------------------------------
-- Offer Datum
-------------------------------------------------
data OfferDatum = OfferDatum
  -- | The policy id for the negotiation beacon script.
  { negotiationBeaconId :: NegotiationBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The prefixed lender's staking credential as a token name.
  , lenderId :: LenderId
  -- | The lender's address.
  , lenderAddress :: Address
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeaconId :: AssetBeaconId
  -- | The size of the loan.
  , loanPrincipal :: Integer
  -- | The frequency at which interest must be applied and/or penalties applied.
  , epochDuration :: Maybe PlutusTime
  -- | How long the loan is active once accepted.
  , loanTerm :: PlutusTime
  -- | The interest that must be periodically applied.
  , loanInterest :: Fraction
  -- | Whether the interest is compounding.
  , compoundingInterest  :: Bool
  -- | The minimum loan payment that must be made each loan epoch.
  , minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , penalty :: Penalty
  -- | The relative values of the collateral assets.
  , collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , claimPeriod :: PlutusTime
  -- | How much ADA was used for the UTxO's minUTxOValue.
  , offerDeposit :: Integer
  -- | An optional offer expiration time.
  , offerExpiration :: Maybe PlutusTime
  } deriving (Eq,Show)

makeFieldLabelsNoPrefix ''OfferDatum

instance Default OfferDatum where
  def = OfferDatum
    { negotiationBeaconId = ""
    , activeBeaconId = ""
    , lenderId = ""
    , lenderAddress = Address (PubKeyCredential "") Nothing
    , loanAsset = Asset ("","")
    , assetBeaconId = ""
    , loanPrincipal = 0
    , epochDuration = Nothing
    , loanTerm = 0
    , loanInterest = Fraction (0,1)
    , compoundingInterest = False
    , minPayment = 0
    , penalty = NoPenalty
    , collateralization = Collateralization []
    , collateralIsSwappable = False
    , claimPeriod = 0
    , offerDeposit = 0
    , offerExpiration = Nothing
    }

instance ToJSON OfferDatum where
  toJSON OfferDatum{..} =
    object [ "negotiation_beacon_id" .= negotiationBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "lender_id" .= lenderId
           , "lender_address" .= lenderAddress
           , "loan_asset" .= loanAsset
           , "asset_beacon_id" .= assetBeaconId
           , "principal" .= loanPrincipal
           , "epoch_duration" .= epochDuration
           , "loan_term" .= loanTerm
           , "loan_interest" .= loanInterest
           , "compounding_interest" .= compoundingInterest
           , "minimum_payment" .= minPayment
           , "penalty" .= penalty
           , "collateralization" .= collateralization
           , "collateral_is_swappable" .= collateralIsSwappable
           , "claim_period" .= claimPeriod
           , "offer_deposit" .= offerDeposit
           , "offer_expiration" .= offerExpiration
           ]

instance FromJSON OfferDatum where
  parseJSON = withObject "OfferDatum" $ \o ->
    OfferDatum
      <$> (o .: "negotiation_beacon_id")
      <*> (o .: "active_beacon_id")
      <*> (o .: "lender_id")
      <*> (o .: "lender_address")
      <*> (o .: "loan_asset")
      <*> (o .: "asset_beacon_id")
      <*> (o .: "principal")
      <*> (o .: "epoch_duration")
      <*> (o .: "loan_term")
      <*> (o .: "loan_interest")
      <*> (o .: "compounding_interest")
      <*> (o .: "minimum_payment")
      <*> (o .: "penalty")
      <*> (o .: "collateralization")
      <*> (o .: "collateral_is_swappable")
      <*> (o .: "claim_period")
      <*> (o .: "offer_deposit")
      <*> (o .: "offer_expiration")

-------------------------------------------------
-- Active Datum
-------------------------------------------------
data ActiveDatum = ActiveDatum
  -- | The policy id for the active beacon script.
  { activeBeaconId :: ActiveBeaconId
  -- | The hash for the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The hash for the interest observer script.
  , interestObserverHash :: ScriptHash
  -- | The hash for the address update observer script.
  , addressUpdateObserverHash :: ScriptHash
  -- | The borrower's staking credential as a token name.
  , borrowerId :: BorrowerId
  -- | The lender's address.
  , lenderAddress :: Address
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeaconId :: AssetBeaconId
  -- | The size of the loan.
  , loanPrincipal :: Integer
  -- | The frequency at which interest/penalties must be applied.
  , epochDuration :: Maybe PlutusTime
  -- | The last time interest/penalties were applied.
  , lastEpochBoundary :: PV2.POSIXTime
  -- | How long the loan is active once accepted.
  , loanTerm :: PlutusTime
  -- | The interest that must be periodically applied.
  , loanInterest :: Fraction
  -- | Whether the interest is compounding.
  , compoundingInterest  :: Bool
  -- | The minimum loan partial payment that can be made.
  , minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , penalty :: Penalty
  -- | The relative values of the collateral assets.
  , collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , claimExpiration :: PlutusTime
  -- | The time at which the loan will expire.
  , loanExpiration :: PlutusTime
  -- | The loan's remaining unpaid balance.
  , loanOutstanding :: Fraction
  -- | The total payments made this loan epoch.
  , totalEpochPayments :: Integer
  -- | The loan's unique indentifier.
  , loanId :: LoanId
  } deriving (Eq,Show)

makeFieldLabelsNoPrefix ''ActiveDatum

instance ToJSON ActiveDatum where
  toJSON ActiveDatum{..} =
    object [ "active_beacon_id" .= activeBeaconId
           , "payment_observer_script" .= paymentObserverHash
           , "interest_observer_script" .= interestObserverHash
           , "address_update_observer_script" .= addressUpdateObserverHash
           , "borrower_id" .= borrowerId
           , "lender_address" .= lenderAddress
           , "loan_asset" .= loanAsset
           , "asset_beacon_id" .= assetBeaconId
           , "principal" .= loanPrincipal
           , "epoch_duration" .= epochDuration
           , "last_compounding" .= lastEpochBoundary
           , "loan_term" .= loanTerm
           , "loan_interest" .= loanInterest
           , "compounding_interest" .= compoundingInterest
           , "minimum_payment" .= minPayment
           , "penalty" .= penalty
           , "collateralization" .= collateralization
           , "collateral_is_swappable" .= collateralIsSwappable
           , "claim_expiration" .= claimExpiration
           , "loan_expiration" .= loanExpiration
           , "loan_outstanding" .= loanOutstanding
           , "total_payment_this_epoch" .= totalEpochPayments
           , "loan_id" .= loanId
           ]

instance FromJSON ActiveDatum where
  parseJSON = withObject "ActiveDatum" $ \o ->
    ActiveDatum
      <$> (o .: "active_beacon_id")
      <*> (o .: "payment_observer_script")
      <*> (o .: "interest_observer_script")
      <*> (o .: "address_update_observer_script")
      <*> (o .: "borrower_id")
      <*> (o .: "lender_address")
      <*> (o .: "loan_asset")
      <*> (o .: "asset_beacon_id")
      <*> (o .: "principal")
      <*> (o .: "epoch_duration")
      <*> (o .: "last_compounding")
      <*> (o .: "loan_term")
      <*> (o .: "loan_interest")
      <*> (o .: "compounding_interest")
      <*> (o .: "minimum_payment")
      <*> (o .: "penalty")
      <*> (o .: "collateralization")
      <*> (o .: "collateral_is_swappable")
      <*> (o .: "claim_expiration")
      <*> (o .: "loan_expiration")
      <*> (o .: "loan_outstanding")
      <*> (o .: "total_payment_this_epoch")
      <*> (o .: "loan_id")

instance Default ActiveDatum where
  def = ActiveDatum
    { activeBeaconId = ""
    , paymentObserverHash = ""
    , interestObserverHash = ""
    , addressUpdateObserverHash = ""
    , borrowerId = ""
    , lenderAddress = Address (PubKeyCredential "") Nothing
    , loanAsset = Asset ("","")
    , assetBeaconId = ""
    , loanPrincipal = 0
    , epochDuration = Nothing
    , lastEpochBoundary = 0
    , loanTerm = 0
    , loanInterest = Fraction (0,1)
    , compoundingInterest = False
    , minPayment = 0
    , penalty = NoPenalty
    , collateralization = Collateralization []
    , collateralIsSwappable = False
    , claimExpiration = 0
    , loanExpiration = 0
    , loanOutstanding = 0
    , totalEpochPayments = 0
    , loanId = ""
    }

-------------------------------------------------
-- Payment Datum
-------------------------------------------------
newtype PaymentDatum = PaymentDatum (CurrencySymbol,TokenName)
  deriving (Show,Eq)

instance ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,tok])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData tok
  fromBuiltinData _ = Nothing

-------------------------------------------------
-- Loan Spending Redeemer
-------------------------------------------------
data LoanRedeemer
  -- | Close or update an Ask UTxO.
  = CloseOrUpdateAsk
  -- | Close or update an Offer UTxO.
  | CloseOrUpdateOffer
  -- | Convert an Ask UTxO and an Offer UTxO into an Active UTxO.
  | AcceptOffer
  -- | Make a payment on a loan. The amount is the size of the payment.
  | MakePayment { paymentAmount :: Integer }
  -- | Apply interest to a loan N times and deposit the specified amount of ada if needed.
  | ApplyInterest { depositIncrease :: Integer, numberOfTimes :: Integer }
  -- | Claim collateral for an expired loan using the Key NFT.
  | SpendWithKeyNFT
  -- | Update the address where loan payments must go. Optionally deposit additional ada if needed.
  | UpdateLenderAddress { newAddress :: Address, depositIncrease :: Integer }
  -- | Claim "Lost" collateral.
  | Unlock
  deriving (Generic,FromJSON,ToJSON,Eq,Show)

instance Display LoanRedeemer where
  display CloseOrUpdateAsk = "Closed/Updated Ask"
  display CloseOrUpdateOffer = "Closed/Updated Offer"
  display AcceptOffer = "Accepted"
  display (MakePayment _) = "Made Payment"
  display (ApplyInterest _ _) = "Applied Interest/Penalties"
  display SpendWithKeyNFT = "Lender Claimed Collateral"
  display (UpdateLenderAddress _ _) = "Address Updated"
  display Unlock = "Borrower Claimed Lost Collateral"

-------------------------------------------------
-- Payment Observer Redeemer
-------------------------------------------------
data PaymentObserverRedeemer
  -- | Observer a borrower's loan payment transaction.
  = ObservePayment
  -- | Register the script.
  | RegisterPaymentObserverScript
  deriving (Eq,Show)

-------------------------------------------------
-- Interest Observer Redeemer
-------------------------------------------------
data InterestObserverRedeemer
  -- | Observer a borrower's loan interest application transaction.
  = ObserveInterest
  -- | Register the script.
  | RegisterInterestObserverScript
  deriving (Eq,Show)

-------------------------------------------------
-- Address Update Observer Redeemer
-------------------------------------------------
data AddressUpdateObserverRedeemer
  -- | Observer a lender's address update transaction.
  = ObserveAddressUpdate
  -- | Register the script.
  | RegisterAddressUpdateObserverScript
  deriving (Eq,Show)

-------------------------------------------------
-- Negotiation Beacon Redeemer
-------------------------------------------------
data NegotiationBeaconsRedeemer
  -- | Create, close, or update some Ask UTxOs (1 or more). The credential is the borrower's
  -- staking credential.
  = CreateCloseOrUpdateAsk { borrowerStakeCredential :: Credential }
  -- | Create, close, or update some Offer UTxOs (1 or more). The credential is the lender's
  -- staking credential.
  | CreateCloseOrUpdateOffer { lenderStakeCredential :: Credential }
  -- | Burn any beacons. This can only be used in the same transaction where CreateActive is used
  -- for the active beacon script.
  | BurnNegotiationBeacons
  -- | Register the script.
  | RegisterNegotiationScript
  deriving (Eq,Show)

makePrisms ''NegotiationBeaconsRedeemer

-------------------------------------------------
-- Active Beacon Redeemer
-------------------------------------------------
data ActiveBeaconsRedeemer
  -- | Create some Active UTxOs (1 or more) for the borrower. The CurrencySymbol is the 
  -- policy id for the negotiation beacons.
  = CreateActive { negotiationPolicyId :: CurrencySymbol }
  -- | Burn the lock and key NFT to claim expired collateral.
  | BurnKeyAndClaimExpired
  -- Burn all beacons and claim "Lost" collateral.
  | BurnAndUnlockLost
  -- | Burn any beacons.
  | BurnActiveBeacons
  deriving (Eq,Show,Generic,FromJSON,ToJSON)

PlutusTx.makeIsDataIndexed ''AskDatum [('AskDatum,0)]
PlutusTx.makeIsDataIndexed ''OfferDatum [('OfferDatum,1)]
PlutusTx.makeIsDataIndexed ''ActiveDatum [('ActiveDatum,2)]
PlutusTx.unstableMakeIsData ''LoanRedeemer
PlutusTx.unstableMakeIsData ''PaymentObserverRedeemer
PlutusTx.unstableMakeIsData ''InterestObserverRedeemer
PlutusTx.unstableMakeIsData ''AddressUpdateObserverRedeemer
PlutusTx.unstableMakeIsData ''NegotiationBeaconsRedeemer
PlutusTx.unstableMakeIsData ''ActiveBeaconsRedeemer

-------------------------------------------------
-- Smart Contracts
-------------------------------------------------
-- | The proxy script where lender payments can go to be protected by a smart contract staking
-- credential.
proxyScript :: SerialisedScript
proxyScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.proxy_script"

proxyScriptSize :: Integer
proxyScriptSize = getScriptSize proxyScript

-- | The hash of the proxy script.
proxyScriptHash :: ScriptHash
proxyScriptHash = hashScript proxyScript

-- | The loan spending script. All borrower addresses use this script for the payment credential.
loanScript :: SerialisedScript
loanScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.loan_script"

loanScriptSize :: Integer
loanScriptSize = getScriptSize loanScript

-- | The hash of the loan script.
loanScriptHash :: ScriptHash
loanScriptHash = hashScript loanScript

-- | The script responsible for observing loan payments to lenders.
paymentObserverScript :: SerialisedScript
paymentObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.payment_observer_script")
    [PV2.toData loanScriptHash]

paymentObserverScriptSize :: Integer
paymentObserverScriptSize = getScriptSize paymentObserverScript

-- | The hash of the payment observer script.
paymentObserverScriptHash :: ScriptHash
paymentObserverScriptHash = hashScript paymentObserverScript

-- | The script responsible for observing interest updates.
interestObserverScript :: SerialisedScript
interestObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.interest_observer_script")
    [PV2.toData loanScriptHash]

interestObserverScriptSize :: Integer
interestObserverScriptSize = getScriptSize interestObserverScript

-- | The hash of the interest observer script.
interestObserverScriptHash :: ScriptHash
interestObserverScriptHash = hashScript interestObserverScript

-- | The script responsible for observing lender address updates.
addressUpdateObserverScript :: SerialisedScript
addressUpdateObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.address_update_observer_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData loanScriptHash
    ]

addressUpdateObserverScriptSize :: Integer
addressUpdateObserverScriptSize = getScriptSize addressUpdateObserverScript

-- | The hash of the address update observer script.
addressUpdateObserverScriptHash :: ScriptHash
addressUpdateObserverScriptHash = hashScript addressUpdateObserverScript

-- | The beacon script responsible for the active phase beacons.
activeBeaconScript :: SerialisedScript
activeBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.active_beacon_script")
    [ PV2.toData loanScriptHash
    , PV2.toData paymentObserverScriptHash
    , PV2.toData interestObserverScriptHash
    , PV2.toData addressUpdateObserverScriptHash
    ]

activeBeaconScriptSize :: Integer
activeBeaconScriptSize = getScriptSize activeBeaconScript

-- | The hash of the active beacon script.
activeBeaconScriptHash :: ScriptHash
activeBeaconScriptHash = hashScript activeBeaconScript

-- | The policy id for the active beacon script.
activeBeaconCurrencySymbol :: CurrencySymbol
activeBeaconCurrencySymbol = scriptHashToPolicyId activeBeaconScriptHash

-- | The beacon script responsible for the negotation phase beacons.
negotiationBeaconScript :: SerialisedScript
negotiationBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.negotiation_beacon_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData loanScriptHash
    , PV2.toData activeBeaconScriptHash
    ]

-- | The beacon script responsible for the negotation phase beacons.
negotiationBeaconScriptSize :: Integer
negotiationBeaconScriptSize = getScriptSize negotiationBeaconScript

-- | The hash of the negotation beacon script.
negotiationBeaconScriptHash :: ScriptHash
negotiationBeaconScriptHash = hashScript negotiationBeaconScript

-- | The policy id for the negotation beacon script.
negotiationBeaconCurrencySymbol :: CurrencySymbol
negotiationBeaconCurrencySymbol = scriptHashToPolicyId negotiationBeaconScriptHash

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Create the Asset beacon name for the loan asset.
genLoanAssetBeaconName :: Asset -> AssetBeaconId
genLoanAssetBeaconName (Asset (CurrencySymbol sym, TokenName name)) =
  AssetBeaconId $ TokenName $ PlutusTx.sha2_256 $ "Asset" <> sym <> name

-- | Create the loan id from the offer's output reference.
genLoanId :: TxOutRef -> LoanId
genLoanId (TxOutRef (TxId txHash) index) = 
  let TokenName index' = show index
  in LoanId $ TokenName $ PlutusTx.sha2_256 $ txHash <> index'

-- | Create the prefixed LenderId from the lender's staking credential.
genLenderId :: Credential -> LenderId
genLenderId cred = LenderId $ TokenName $ case cred of
  PubKeyCredential (PubKeyHash pkh) -> unsafeToBuiltinByteString "00" <> pkh
  ScriptCredential (ScriptHash sh) -> unsafeToBuiltinByteString "01" <> sh

-- | Create the BorrowerId from the borrower's staking credential.
genBorrowerId :: Credential -> BorrowerId
genBorrowerId cred = BorrowerId $ TokenName $ case cred of
  PubKeyCredential (PubKeyHash pkh) -> pkh
  ScriptCredential (ScriptHash sh) -> sh

askBeaconName :: TokenName
askBeaconName = "Ask"

offerBeaconName :: TokenName
offerBeaconName = "Offer"

activeBeaconName :: TokenName
activeBeaconName = "Active"

-------------------------------------------------
-- Calculations
-------------------------------------------------
type Balance = Fraction
type Interest = Fraction

-- | Apply interest to the loan's current outstanding balance. The calculation is:
-- balance * (1 + interest).
applyInterest :: Balance -> Interest -> Balance
applyInterest (Fraction (balNum,balDen)) (Fraction (interestNum,interestDen)) =
    Fraction (totalNum `div` newGcd, totalDen `div` newGcd)
  where
    totalNum = balNum * (interestDen + interestNum) -- Balance * (1 + interest)
    totalDen = balDen * interestDen
    newGcd = PlutusTx.gcd totalNum totalDen

-- | Subtract the payment amount from the loan's outstanding balance.
subtractPayment :: Integer -> Balance -> Balance
subtractPayment paymentAmount (Fraction (balNum,balDen)) =
    Fraction (totalNum, balDen)
  where
    totalNum = balNum - balDen * paymentAmount

-- | Apply interest N times and apply the penalty when necessary.
applyInterestNTimes :: Bool -> Penalty -> Integer -> Interest -> Balance -> Balance
applyInterestNTimes _ _ 0 _ (Fraction (balNum,balDen)) =
  let newGcd = PlutusTx.gcd balNum balDen
  in Fraction (balNum `div` newGcd, balDen `div` newGcd)
applyInterestNTimes 
  penalize 
  penalty 
  count 
  interest@(Fraction (interestNum,interestDen)) 
  (Fraction (balNum,balDen)) =
    if penalize then case penalty of
      NoPenalty ->
        applyInterestNTimes True penalty (count - 1) interest $ 
          Fraction (balNum * (interestDen + interestNum), balDen * interestDen)
      FixedFee fee ->
        applyInterestNTimes True penalty (count - 1) interest $ 
          Fraction 
            ( (balNum + (fee * balDen)) * (interestDen + interestNum)
            , balDen * interestDen
            )
      PercentFee (Fraction (feeNum,feeDen)) ->
        let (newBalNum,newBalDen) = (balNum * (feeDen + feeNum), balDen * feeDen)
        in applyInterestNTimes True penalty (count - 1) interest $ 
            Fraction 
              ( newBalNum * (interestDen + interestNum)
              , newBalDen * interestDen
              )
    else
      applyInterestNTimes True penalty (count - 1) interest $ 
        Fraction (balNum * (interestDen + interestNum), balDen * interestDen)

-------------------------------------------------
-- Creating datums
-------------------------------------------------
-- | Create a new AskDatum.
createAskDatum 
  :: Credential -- ^ The borrower's staking credential 
  -> NativeAsset -- ^ Loan Asset + amount
  -> PlutusTime -- ^ Loan Term
  -> [NativeAsset] -- ^ Collateral
  -> AskDatum
createAskDatum borrowerCredential loanAsset duration collateral = 
  let convertedLoanAsset = fromNativeAsset loanAsset in
    AskDatum
      { negotiationBeaconId = NegotiationBeaconId negotiationBeaconCurrencySymbol
      , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
      , borrowerId = genBorrowerId borrowerCredential
      , loanAsset = convertedLoanAsset
      , assetBeaconId = genLoanAssetBeaconName convertedLoanAsset
      , loanPrincipal = loanAsset ^. #quantity
      , loanTerm = duration
      , collateral = Collateral $ map fromNativeAsset collateral
      }

data LenderTerms = LenderTerms
  { lenderCredential :: Credential
  , lenderAddress :: PaymentAddress
  , loanTerm :: PlutusTime -- A counter-offer for the loan duration.
  , loanAmount :: NativeAsset -- A counter-offer for the asset/amount to borrow.
  , interest :: Rational
  , compoundingInterest :: Bool
  , epochDuration :: Maybe PlutusTime
  , minPayment :: Integer
  , penalty :: Penalty
  , claimPeriod :: PlutusTime
  , offerExpiration :: Maybe PlutusTime
  , collateralization :: [(NativeAsset,Rational)]
  , offerDeposit :: Integer
  , collateralIsSwappable :: Bool
  } deriving (Show,Eq)

-- | Create a new OfferDatum.
createOfferDatum :: LenderTerms -> OfferDatum
createOfferDatum LenderTerms{..} = OfferDatum
  { negotiationBeaconId = NegotiationBeaconId negotiationBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , lenderId = genLenderId lenderCredential
  , lenderAddress = 
      fromRight (Address (PubKeyCredential "") Nothing) $ paymentAddressToPlutusAddress lenderAddress
  , loanAsset = fromNativeAsset loanAmount
  , assetBeaconId = genLoanAssetBeaconName $ fromNativeAsset loanAmount
  , loanPrincipal = loanAmount ^. #quantity
  , loanTerm = loanTerm -- use the new loan term.
  , loanInterest = fromRational interest
  , compoundingInterest = compoundingInterest
  , epochDuration = epochDuration
  , minPayment = minPayment
  , penalty = penalty
  , collateralIsSwappable = collateralIsSwappable
  , claimPeriod = claimPeriod
  , offerDeposit = offerDeposit
  , offerExpiration = offerExpiration
  , collateralization = 
      Collateralization $ map (over _2 fromRational . over _1 fromNativeAsset) collateralization
  }

-- | Create an ActiveDatum from an OfferDatum, offer output reference, BorrowerId, and loan 
-- start time.
createActiveDatumFromOffer :: Credential -> TxOutRef -> PlutusTime -> OfferDatum -> ActiveDatum
createActiveDatumFromOffer borrowerCred offerId startTime OfferDatum{..} = ActiveDatum
  { activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , paymentObserverHash = paymentObserverScriptHash
  , interestObserverHash = interestObserverScriptHash
  , addressUpdateObserverHash = addressUpdateObserverScriptHash
  , borrowerId = genBorrowerId borrowerCred
  , lenderAddress = lenderAddress
  , loanAsset = loanAsset
  , assetBeaconId = assetBeaconId
  , loanPrincipal = loanPrincipal
  , epochDuration = epochDuration
  , loanTerm = loanTerm
  , loanInterest = loanInterest
  , compoundingInterest = compoundingInterest
  , minPayment = minPayment
  , penalty = penalty
  , collateralization = collateralization
  , collateralIsSwappable = collateralIsSwappable
  , lastEpochBoundary = startTime
  , claimExpiration = startTime + loanTerm + claimPeriod
  , loanExpiration = startTime + loanTerm
  , loanOutstanding = applyInterest (fromInteger loanPrincipal) loanInterest
  , totalEpochPayments = 0
  , loanId = genLoanId offerId
  }

-- | Update the ActiveDatum to reflect the payment.
createPostPaymentActiveDatum :: Integer -> ActiveDatum -> ActiveDatum
createPostPaymentActiveDatum paymentAmount activeDatum =
  activeDatum
    -- Subtract the payment amount.
    & #loanOutstanding %~ subtractPayment paymentAmount
    -- Increase the amount paid this epoch.
    & #totalEpochPayments %~ (+ paymentAmount)

-- | Update the ActiveDatum to reflect the interest accrual and penalties.
createPostInterestActiveDatum :: Integer -> ActiveDatum -> ActiveDatum
createPostInterestActiveDatum numberOfTimes activeDatum@ActiveDatum{..} =
  let correctedInterest = if compoundingInterest then loanInterest else 0 in
    activeDatum
      -- Add the epochDuration to the lastEpochBoundary field. Also account for the
      -- interest being applied for several compound periods in a single transaction.
      & #lastEpochBoundary %~ (+ maybe 0 (* fromInteger numberOfTimes) epochDuration)
      -- Apply the interest n time. The penalty must be applied the first time as well if the
      -- minimum payment has not been met.
      & #loanOutstanding %~ 
          applyInterestNTimes 
            (minPayment > totalEpochPayments) 
            penalty 
            numberOfTimes 
            correctedInterest
      -- Reset the totalEpochPayments field.
      & #totalEpochPayments .~ 0

-- | Update the ActiveDatum to reflect the lender address update.
createPostAddressUpdateActiveDatum :: PlutusAddress -> ActiveDatum -> ActiveDatum
createPostAddressUpdateActiveDatum = set #lenderAddress

-------------------------------------------------
-- Protocol Addresses
-------------------------------------------------
-- | Check whether the address is a valid lender payment address.
isValidLoanPaymentAddress :: Address -> Bool
isValidLoanPaymentAddress (Address (PubKeyCredential _) _) = True
isValidLoanPaymentAddress (Address (ScriptCredential cred) mStakeCred) =
  cred == proxyScriptHash && case mStakeCred of
    Just (StakingHash _) -> True
    _ -> False

-- | Create the loan address for the stake credential.
genLoanAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genLoanAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential loanScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a loan address.
isLoanAddress :: PaymentAddress -> Bool
isLoanAddress = either (const False) ((== ScriptCredential loanScriptHash) . addressCredential)
              . paymentAddressToPlutusAddress

-- | Create the proxy address for the stake credential.
genProxyAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genProxyAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential proxyScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a proxy address.
isProxyAddress :: PaymentAddress -> Bool
isProxyAddress = either (const False) ((== ScriptCredential proxyScriptHash) . addressCredential)
              . paymentAddressToPlutusAddress

-- | The address that must be used for the negotiation beacon staking execution.
negotiationBeaconStakeAddress :: Network -> StakeAddress
negotiationBeaconStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential negotiationBeaconScriptHash
      , addressStakingCredential = Just $ StakingHash $ ScriptCredential negotiationBeaconScriptHash
      }

-- | The address that must be used for the payment observer staking execution.
paymentObserverStakeAddress :: Network -> StakeAddress
paymentObserverStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential paymentObserverScriptHash
      , addressStakingCredential = Just $ StakingHash $ ScriptCredential paymentObserverScriptHash
      }

-- | The address that must be used for the interest observer staking execution.
interestObserverStakeAddress :: Network -> StakeAddress
interestObserverStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential interestObserverScriptHash
      , addressStakingCredential = Just $ StakingHash $ ScriptCredential interestObserverScriptHash
      }

-- | The address that must be used for the address update observer staking execution.
addressUpdateObserverStakeAddress :: Network -> StakeAddress
addressUpdateObserverStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential addressUpdateObserverScriptHash
      , addressStakingCredential = 
          Just $ StakingHash $ ScriptCredential addressUpdateObserverScriptHash
      }

-------------------------------------------------
-- Reference Script UTxOs
-------------------------------------------------
-- The reference scripts are locked at the loan address without any staking credential.
-- For testnet, that address is: 
-- addr_test1wzc8hkkr9ygdfs02gf0d4hu8awlp8yeefm3kvglf0cw3fnqry5lx4
--
-- The scripts are deliberately stored with an invalid datum so that they are locked forever.

-- | The keys used in the reference script map.
data LoanScriptType
  = LoanScript
  | PaymentObserverScript
  | InterestObserverScript
  | AddressObserverScript
  | NegotiationScript
  | ActiveScript
  | ProxyScript
  deriving (Eq,Ord)

getScriptRef :: Network -> LoanScriptType -> (TxOutRef,Integer)
getScriptRef network scriptType = (referenceScriptMap Map.! network) Map.! scriptType

referenceScriptMap :: Map.Map Network (Map.Map LoanScriptType (TxOutRef,Integer))
referenceScriptMap = Map.fromList
  [ (Testnet, Map.fromList
        [ (LoanScript, (loanScriptTestnetRef, loanScriptSize))
        , (PaymentObserverScript, (paymentObserverScriptTestnetRef, paymentObserverScriptSize))
        , (InterestObserverScript, (interestObserverScriptTestnetRef, interestObserverScriptSize))
        , (AddressObserverScript, (addressUpdateObserverScriptTestnetRef, addressUpdateObserverScriptSize))
        , (NegotiationScript, (negotiationBeaconScriptTestnetRef, negotiationBeaconScriptSize))
        , (ActiveScript, (activeBeaconScriptTestnetRef, activeBeaconScriptSize))
        , (ProxyScript, (proxyScriptTestnetRef, proxyScriptSize))
        ])
  ]

loanScriptTestnetRef :: TxOutRef
loanScriptTestnetRef = 
  TxOutRef "69049298b1b240955e56775d586542cc5b246d3aa0a6843a9c26c24b263a256e" 0

negotiationBeaconScriptTestnetRef :: TxOutRef
negotiationBeaconScriptTestnetRef = 
  TxOutRef "1cf071ff8dd90288de651fd6b774f1e6cc4959faf52cb1261b9b429b1710f8f1" 0

activeBeaconScriptTestnetRef :: TxOutRef
activeBeaconScriptTestnetRef = 
  TxOutRef "7d85aa644fece35ce4d22a5968f08b7f8bd90d83a51ba011f920324afc59f1b0" 0

paymentObserverScriptTestnetRef :: TxOutRef
paymentObserverScriptTestnetRef = 
  TxOutRef "8119bc8f8f6fd9aba3d5b26e36d01cd7794c008140644c22613223cc8c01ceef" 0

interestObserverScriptTestnetRef :: TxOutRef
interestObserverScriptTestnetRef = 
  TxOutRef "272e49fdf315a90a6fbdd3e2ce2593877b6a5af8c71eabecd0c32210b6eed4cf" 0

addressUpdateObserverScriptTestnetRef :: TxOutRef
addressUpdateObserverScriptTestnetRef = 
  TxOutRef "2a02137b2fb0e2a802415160bee0c361dd48e9a0f11df434fda0c22d77fa7b13" 0

proxyScriptTestnetRef :: TxOutRef
proxyScriptTestnetRef = 
  TxOutRef "d47cc4fa725feed453a1f598001fd0daf8684ae28a5f48c00d245eff7e210348" 0
