{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoOptions
  ( -- * Datums
    ProposalDatum(..)
  , ActiveDatum(..)
  , PaymentDatum(..)

    -- * Redeemers
  , OptionsRedeemer(..)
  , ProposalBeaconsRedeemer(..)
  , ActiveBeaconsRedeemer(..)
  , AddressObserverRedeemer(..)
  , _CreateCloseOrUpdateProposals

    -- * Contracts
  , proxyScript
  , proxyScriptHash
  , proxyScriptSize
  , optionsScript
  , optionsScriptHash
  , optionsScriptSize
  , addressObserverScript
  , addressObserverScriptHash
  , addressObserverScriptSize
  , activeBeaconScript
  , activeBeaconScriptHash
  , activeBeaconScriptSize
  , activeBeaconCurrencySymbol
  , proposalBeaconScript
  , proposalBeaconScriptHash
  , proposalBeaconScriptSize
  , proposalBeaconCurrencySymbol

    -- * Beacon Names
  , genContractId
  , genOfferBeaconName
  , genAskBeaconName
  , genPremiumBeaconName
  , genTradingPairBeaconName

    -- * Creating Datums
  , NewProposalInfo(..)
  , createProposalDatum
  , createActiveDatumFromProposal
  , createPostAddressUpdateActiveDatum

    -- * Protocol Addresses
  , isValidOptionsPaymentAddress
  , genWriterAddress
  , isWriterAddress
  , genProxyAddress
  , isProxyAddress
  , proposalBeaconStakeAddress
  , addressObserverStakeAddress

    -- * Reference Scripts Output References
  , getScriptRef

    -- * Re-exports
  , module P2PWallet.Data.DeFi.CardanoOptions.Internal
  ) where

import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.List ((!!))

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusLedgerApi.V2 as PV2

import CardanoOptions.Blueprints

import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.DeFi.CardanoOptions.Internal
import P2PWallet.Data.DeFi.CardanoLoans 
  ( proxyScript
  , proxyScriptHash
  , proxyScriptSize
  , genProxyAddress
  , isProxyAddress
  , proxyScriptTestnetRef
  )
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Proposal Datum
-------------------------------------------------
-- | The datum for an options contract that is available for purchase.
data ProposalDatum = ProposalDatum
  -- | The policy id for the proposal beacon script.
  { proposalBeaconId :: ProposalBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The asset being offered.
  , offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The token name for the trading pair beacon.
  , tradingPairBeacon :: TradingPairBeacon
  -- | The token name for the offer beacon.
  , offerBeacon :: OfferBeacon
  -- | The token name for the ask beacon.
  , askBeacon :: AskBeacon
  -- | The asset the premium must be paid in.
  , premiumAsset :: PremiumAsset
  -- | The token name for the premium asset beacon.
  , premiumBeacon :: PremiumBeacon
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The possible terms the buyer can pick from.
  , possibleTerms :: [Terms]
  } deriving (Eq,Show)

PlutusTx.makeIsDataIndexed ''ProposalDatum [('ProposalDatum,0)]
makeFieldLabelsNoPrefix ''ProposalDatum

instance Default ProposalDatum where
  def = ProposalDatum
    { proposalBeaconId = ""
    , activeBeaconId = ""
    , offerAsset = OfferAsset ("","")
    , offerQuantity = 0
    , askAsset = AskAsset ("","")
    , tradingPairBeacon = ""
    , offerBeacon = ""
    , askBeacon = ""
    , premiumAsset = PremiumAsset ("","")
    , premiumBeacon = ""
    , contractDeposit = 0
    , paymentAddress = Address (PubKeyCredential "") Nothing
    , possibleTerms = []
    }

instance ToJSON ProposalDatum where
  toJSON ProposalDatum{..} =
    object [ "proposal_beacon_id" .= proposalBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "offer_asset" .= offerAsset
           , "offer_quantity" .= offerQuantity
           , "ask_asset" .= askAsset
           , "trading_pair_beacon" .= tradingPairBeacon
           , "offer_beacon" .= offerBeacon
           , "ask_beacon" .= askBeacon
           , "premium_asset" .= premiumAsset
           , "premium_beacon" .= premiumBeacon
           , "contract_deposit" .= contractDeposit
           , "payment_address" .= paymentAddress
           , "possible_terms" .= possibleTerms
           ]

instance FromJSON ProposalDatum where
  parseJSON = withObject "ProposalDatum" $ \o ->
    ProposalDatum 
      <$> o .: "proposal_beacon_id"
      <*> o .: "active_beacon_id"
      <*> o .: "offer_asset"
      <*> o .: "offer_quantity"
      <*> o .: "ask_asset"
      <*> o .: "trading_pair_beacon"
      <*> o .: "offer_beacon"
      <*> o .: "ask_beacon"
      <*> o .: "premium_asset"
      <*> o .: "premium_beacon"
      <*> o .: "contract_deposit"
      <*> o .: "payment_address"
      <*> o .: "possible_terms"

-------------------------------------------------
-- Active Datum
-------------------------------------------------
-- | The datum for an options contract that has been purchased, and can be executed any time up
-- until the expiration.
data ActiveDatum = ActiveDatum
  -- | The policy id for the proposal beacon script. This is needed to support using the same 
  -- active beacons redeemer for purchases, executions, and closing expired.
  { proposalBeaconId :: ProposalBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The hash of the address update observer script.
  , addressObserverHash :: ScriptHash
  -- | The asset being offered.
  , offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The token name for the trading pair beacon.
  , tradingPairBeacon :: TradingPairBeacon
  -- | The token name for the offer beacon.
  , offerBeacon :: OfferBeacon
  -- | The token name for the ask beacon.
  , askBeacon :: AskBeacon
  -- | The strike price for this contract.
  , strikePrice :: Fraction
  -- | This contract's expiration.
  , expiration :: PlutusTime
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the ask asset must go upon execution of the contract.
  , paymentAddress :: Address
  -- | The unique identitier for this contract.
  , contractId :: ContractId
  } deriving (Eq,Show)

PlutusTx.makeIsDataIndexed ''ActiveDatum [('ActiveDatum,1)]
makeFieldLabelsNoPrefix ''ActiveDatum

instance Default ActiveDatum where
  def = ActiveDatum
    { proposalBeaconId = ""
    , activeBeaconId = ""
    , addressObserverHash = ""
    , offerAsset = OfferAsset ("","")
    , offerQuantity = 0
    , askAsset = AskAsset ("","")
    , tradingPairBeacon = ""
    , offerBeacon = ""
    , askBeacon = ""
    , strikePrice = 0
    , expiration = 0
    , contractDeposit = 0
    , paymentAddress = Address (PubKeyCredential "") Nothing
    , contractId = ""
    }

instance ToJSON ActiveDatum where
  toJSON ActiveDatum{..} =
    object [ "proposal_beacon_id" .= proposalBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "address_observer_hash" .= addressObserverHash
           , "offer_asset" .= offerAsset
           , "offer_quantity" .= offerQuantity
           , "ask_asset" .= askAsset
           , "trading_pair_beacon" .= tradingPairBeacon
           , "offer_beacon" .= offerBeacon
           , "ask_beacon" .= askBeacon
           , "contract_deposit" .= contractDeposit
           , "payment_address" .= paymentAddress
           , "strike_price" .= strikePrice
           , "expiration" .= expiration
           , "contract_id" .= contractId
           ]

instance FromJSON ActiveDatum where
  parseJSON = withObject "ActiveDatum" $ \o ->
    ActiveDatum 
      <$> o .: "proposal_beacon_id"
      <*> o .: "active_beacon_id"
      <*> o .: "address_observer_hash"
      <*> o .: "offer_asset"
      <*> o .: "offer_quantity"
      <*> o .: "ask_asset"
      <*> o .: "trading_pair_beacon"
      <*> o .: "offer_beacon"
      <*> o .: "ask_beacon"
      <*> o .: "strike_price"
      <*> o .: "expiration"
      <*> o .: "contract_deposit"
      <*> o .: "payment_address"
      <*> o .: "contract_id"

-------------------------------------------------
-- Payment Datum
-------------------------------------------------
newtype PaymentDatum = PaymentDatum (ActiveBeaconId,ContractId)
  deriving (Generic)

instance PV2.ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,tok])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData tok
  fromBuiltinData _ = Nothing

-------------------------------------------------
-- On-Chain Redeemers
-------------------------------------------------
data OptionsRedeemer
  -- | Close or update a Proposal UTxO.
  = CloseOrUpdateProposal
  -- | Purchase an options contract by converting a Proposal UTxO into an Active UTxO. The
  -- `desiredTermsIndex` identifies which `Terms` the buyer is purchasing.
  | PurchaseContract { desiredTermsIndex :: Integer }
  -- | Execute an active options contract.
  | ExecuteContract
  -- | Close an active options contract that has expired.
  | CloseExpiredContract
  -- | Update the address where the ask asset must go. Optionally deposit additional ada if needed.
  | UpdatePaymentAddress { newAddress :: Address, depositIncrease :: Integer }
  deriving (Show)

makeFieldLabelsNoPrefix ''OptionsRedeemer

data AddressObserverRedeemer
  -- | Observer a writer's address update transaction.
  = ObserveAddressUpdate
  -- | Register the script.
  | RegisterAddressObserverScript
  deriving (Show)

data ProposalBeaconsRedeemer
  -- | Create, close, or update some Proposal UTxOs (1 or more). 
  = CreateCloseOrUpdateProposals
  -- | Burn any beacons. 
  | BurnProposalBeacons
  -- | Register the script.
  | RegisterProposalScript
  deriving (Show)

makePrisms ''ProposalBeaconsRedeemer

data ActiveBeaconsRedeemer
  -- | Create some Active UTxOs (1 or more) by buying Proposal UTxOs. The CurrencySymbol is the 
  -- policy id for the proposal beacons.
  = PurchaseExecuteOrCloseExpiredContracts { proposalPolicyId :: CurrencySymbol }
  -- | Burn any beacons.
  | BurnActiveBeacons
  deriving (Show)

PlutusTx.unstableMakeIsData ''OptionsRedeemer
PlutusTx.unstableMakeIsData ''AddressObserverRedeemer
PlutusTx.unstableMakeIsData ''ProposalBeaconsRedeemer
PlutusTx.unstableMakeIsData ''ActiveBeaconsRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
optionsScript :: SerialisedScript
optionsScript = parseScriptFromCBOR $ blueprints Map.! "cardano_options.options_script"

optionsScriptHash :: ScriptHash
optionsScriptHash = hashScript optionsScript

optionsScriptSize :: Integer
optionsScriptSize = getScriptSize optionsScript

addressObserverScript :: SerialisedScript
addressObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.address_observer_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData optionsScriptHash
    ]

addressObserverScriptHash :: ScriptHash
addressObserverScriptHash = hashScript addressObserverScript

addressObserverScriptSize :: Integer
addressObserverScriptSize = getScriptSize addressObserverScript

activeBeaconScript :: SerialisedScript
activeBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.active_beacon_script")
    [ PV2.toData optionsScriptHash
    , PV2.toData addressObserverScriptHash
    ]

activeBeaconScriptHash :: ScriptHash
activeBeaconScriptHash = hashScript activeBeaconScript

activeBeaconCurrencySymbol :: CurrencySymbol
activeBeaconCurrencySymbol = scriptHashToPolicyId activeBeaconScriptHash

activeBeaconScriptSize :: Integer
activeBeaconScriptSize = getScriptSize activeBeaconScript

proposalBeaconScript :: SerialisedScript
proposalBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.proposal_beacon_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData optionsScriptHash
    , PV2.toData activeBeaconScriptHash
    ]

proposalBeaconScriptHash :: ScriptHash
proposalBeaconScriptHash = hashScript proposalBeaconScript

proposalBeaconCurrencySymbol :: CurrencySymbol
proposalBeaconCurrencySymbol = scriptHashToPolicyId proposalBeaconScriptHash

proposalBeaconScriptSize :: Integer
proposalBeaconScriptSize = getScriptSize proposalBeaconScript

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Create the contract id from the proposal's output reference.
genContractId :: TxOutRef -> ContractId
genContractId (TxOutRef (TxId txHash) index) = 
  let TokenName index' = show index
  in ContractId $ TokenName $ PlutusTx.sha2_256 $ txHash <> index'

-- | Create the offer beacon name for the offer asset.
genOfferBeaconName :: OfferAsset -> OfferBeacon
genOfferBeaconName (OfferAsset (CurrencySymbol sym,TokenName name)) =
    OfferBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "01"

-- | Create the ask beacon name for the ask asset.
genAskBeaconName :: AskAsset -> AskBeacon
genAskBeaconName (AskAsset (CurrencySymbol sym,TokenName name)) =
    AskBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "02"

-- | Create the premium asset beacon name for the premium asset.
genPremiumBeaconName :: PremiumAsset -> PremiumBeacon
genPremiumBeaconName (PremiumAsset (CurrencySymbol sym,TokenName name)) =
    PremiumBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "03"

-- | Generate the beacon asset name by hashing offer ++ ask. The policy id for
-- ADA is set to "00".
genTradingPairBeaconName :: OfferAsset -> AskAsset -> TradingPairBeacon
genTradingPairBeaconName (OfferAsset assetX) (AskAsset assetY) =
    TradingPairBeacon $ TokenName $ PlutusTx.sha2_256 $ fullName assetX <> fullName assetY
  where
    fullName :: (CurrencySymbol,TokenName) -> BuiltinByteString
    fullName (CurrencySymbol sym, TokenName name)
      | sym == "" = unsafeToBuiltinByteString "00" <> name
      | otherwise = sym <> name

-------------------------------------------------
-- Creating datums
-------------------------------------------------
-- | Required information for creating a new ProposalDatum.
data NewProposalInfo = NewProposalInfo
  -- | The offer asset.
  { offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The asset the premium must be paid in.
  , premiumAsset :: PremiumAsset
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The possible terms the buyer can pick from.
  , possibleTerms :: [Terms]
  } deriving (Show)

-- | Convert the proposal info to the ProposalDatum without checking any invariants. 
createProposalDatum :: NewProposalInfo -> ProposalDatum
createProposalDatum NewProposalInfo{..} = ProposalDatum
  { proposalBeaconId = ProposalBeaconId proposalBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , offerAsset = offerAsset
  , offerQuantity = offerQuantity
  , askAsset = askAsset
  , tradingPairBeacon = genTradingPairBeaconName offerAsset askAsset
  , offerBeacon = genOfferBeaconName offerAsset
  , askBeacon = genAskBeaconName askAsset
  , premiumAsset = premiumAsset
  , premiumBeacon = genPremiumBeaconName premiumAsset
  , contractDeposit = contractDeposit
  , paymentAddress = paymentAddress
  , possibleTerms = possibleTerms
  }

-- | Create an ActiveDatum from a ProposalDatum, its output reference, and the desiredTermsIndex.
createActiveDatumFromProposal :: Integer -> TxOutRef -> ProposalDatum -> ActiveDatum
createActiveDatumFromProposal termsIndex proposalId ProposalDatum{..} = ActiveDatum
    { proposalBeaconId = proposalBeaconId
    , activeBeaconId = activeBeaconId
    , addressObserverHash = addressObserverScriptHash
    , offerAsset = offerAsset
    , offerQuantity = offerQuantity
    , askAsset = askAsset
    , tradingPairBeacon = tradingPairBeacon
    , offerBeacon = offerBeacon
    , askBeacon = askBeacon
    , strikePrice = view #strikePrice desiredTerms
    , expiration = view #expiration desiredTerms
    , contractDeposit = contractDeposit
    , paymentAddress = paymentAddress
    , contractId = genContractId proposalId
    }
  where
    desiredTerms :: Terms
    desiredTerms = possibleTerms !! fromIntegral termsIndex

createPostAddressUpdateActiveDatum :: PlutusAddress -> Integer -> ActiveDatum -> ActiveDatum
createPostAddressUpdateActiveDatum newAddress extraDeposit oldDatum =
  oldDatum
    & #paymentAddress .~ newAddress
    & #contractDeposit %~ (+ extraDeposit)

-------------------------------------------------
-- Protocol Addresses
-------------------------------------------------
-- | Check whether the address is a valid lender payment address.
isValidOptionsPaymentAddress :: Address -> Bool
isValidOptionsPaymentAddress (Address (PubKeyCredential _) _) = True
isValidOptionsPaymentAddress (Address (ScriptCredential cred) mStakeCred) =
  cred == proxyScriptHash && case mStakeCred of
    Just (StakingHash _) -> True
    _ -> False

-- | Create the writer address for the stake credential.
genWriterAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genWriterAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential optionsScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a writer address.
isWriterAddress :: PaymentAddress -> Bool
isWriterAddress = either (const False) ((== ScriptCredential optionsScriptHash) . addressCredential)
                . paymentAddressToPlutusAddress

-- | The address that must be used for the proposal beacon staking execution.
proposalBeaconStakeAddress :: Network -> StakeAddress
proposalBeaconStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential proposalBeaconScriptHash
      , addressStakingCredential = Just $ StakingHash $ ScriptCredential proposalBeaconScriptHash
      }

-- | The address that must be used for the address update observer staking execution.
addressObserverStakeAddress :: Network -> StakeAddress
addressObserverStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential addressObserverScriptHash
      , addressStakingCredential = 
          Just $ StakingHash $ ScriptCredential addressObserverScriptHash
      }

-------------------------------------------------
-- Reference Script UTxOs
-------------------------------------------------
-- The reference scripts are locked at the loan address without any staking credential.
-- For testnet, that address is: 
--
-- The scripts are deliberately stored with an invalid datum so that they are locked forever.

getScriptRef :: Network -> ScriptHash -> (TxOutRef,Integer)
getScriptRef network scriptHash = (referenceScriptMap Map.! network) Map.! scriptHash

referenceScriptMap :: Map.Map Network (Map.Map ScriptHash (TxOutRef,Integer))
referenceScriptMap = Map.fromList
  [ (Testnet, Map.fromList
        [ (optionsScriptHash, (optionsScriptTestnetRef, optionsScriptSize))
        , (proposalBeaconScriptHash, (proposalBeaconScriptTestnetRef, proposalBeaconScriptSize))
        , (activeBeaconScriptHash, (activeBeaconScriptTestnetRef, activeBeaconScriptSize))
        , (addressObserverScriptHash, (addressObserverScriptTestnetRef, addressObserverScriptSize))
        , (proxyScriptHash, (proxyScriptTestnetRef, proxyScriptSize))
        ])
  ]

optionsScriptTestnetRef :: TxOutRef
optionsScriptTestnetRef = 
  TxOutRef "8df8187a76bbfd8b04921d844b7aed52088a20da026d4764c7e9ceeeda79b065" 0

proposalBeaconScriptTestnetRef :: TxOutRef
proposalBeaconScriptTestnetRef = 
  TxOutRef "b2d23610ebc013c63397c73f674ee0522c39e1b10fc80263babff276741f02fe" 0

activeBeaconScriptTestnetRef :: TxOutRef
activeBeaconScriptTestnetRef = 
  TxOutRef "947423f6c830a483c210baf71b173ba53db3b9f838a5e511d1a3e954cdc13f80" 0

addressObserverScriptTestnetRef :: TxOutRef
addressObserverScriptTestnetRef = 
  TxOutRef "8bb773cbd8de20135f8143879895d89291dd99adb04d8d36f0179200fdd79bee" 0
