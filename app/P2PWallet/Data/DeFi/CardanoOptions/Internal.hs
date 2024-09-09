{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoOptions.Internal 
  ( ProposalBeaconId(..)
  , ActiveBeaconId(..)
  , Fraction(..)
  , OfferAsset(..)
  , AskAsset(..)
  , PremiumAsset(..)
  , ContractId(..)
  , TradingPairBeacon(..)
  , OfferBeacon(..)
  , AskBeacon(..)
  , PremiumBeacon(..)
  , Terms(..)
  ) where

import Data.Aeson
import Data.Ratio qualified as Ratio

import qualified PlutusTx
import qualified PlutusLedgerApi.V2 as PV2

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- ProposalBeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the proposal beacon script.
newtype ProposalBeaconId = ProposalBeaconId { unProposalBeaconId :: PV2.CurrencySymbol }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ProposalBeaconId where
  toJSON = toJSON . display

instance FromJSON ProposalBeaconId where
  parseJSON = withText "ProposalBeaconId" $ 
    maybe mzero (return . ProposalBeaconId . CurrencySymbol) . parseHex

makeFieldLabelsNoPrefix ''ProposalBeaconId

-------------------------------------------------
-- ActiveBeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the active beacon script.
newtype ActiveBeaconId = ActiveBeaconId { unActiveBeaconId :: PV2.CurrencySymbol }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ActiveBeaconId where
  toJSON = toJSON . display

instance FromJSON ActiveBeaconId where
  parseJSON = withText "ActiveBeaconId" $ 
    maybe mzero (return . ActiveBeaconId . CurrencySymbol) . parseHex

makeFieldLabelsNoPrefix ''ActiveBeaconId

-------------------------------------------------
-- Fraction
-------------------------------------------------
-- | A wrapper around two integers that make up a fraction. This is used
-- in the absence of a decimal type on change.
newtype Fraction = Fraction { unFraction :: (Integer,Integer) }
  deriving (Show,Eq)

instance Ord Fraction where
  (Fraction (num1,den1)) <= (Fraction (num2,den2)) = num1 * den2 <= num2 * den1

instance ToData Fraction where
  toBuiltinData (Fraction (num,den)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData num, PV2.toData den]

instance FromData Fraction where
  fromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) =
    fmap Fraction . (,) 
      <$> PV2.fromData num 
      <*> PV2.fromData den
  fromBuiltinData _ = Nothing

instance UnsafeFromData Fraction where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) = 
    Fraction (unsafeFromData num, unsafeFromData den)
  unsafeFromBuiltinData _ = error "Could not convert Data to Fraction"

makeFieldLabelsNoPrefix ''Fraction

fractionToRational :: Fraction -> Rational
fractionToRational Fraction{unFraction=(num,den)} = num Ratio.% den

rationalToFraction :: Rational -> Fraction
rationalToFraction rat = Fraction (Ratio.numerator rat, Ratio.denominator rat)

instance Num Fraction where
  frac1 + frac2 = rationalToFraction $ fractionToRational frac1 + fractionToRational frac2
  frac1 * frac2 = rationalToFraction $ fractionToRational frac1 * fractionToRational frac2
  negate = rationalToFraction . negate . fractionToRational
  abs = rationalToFraction . abs . fractionToRational
  fromInteger = Fraction . (,1)
  signum = rationalToFraction . signum . fractionToRational

instance Real Fraction where
  toRational = fractionToRational

instance Fractional Fraction where
  fromRational = rationalToFraction
  recip = rationalToFraction . recip . fractionToRational

instance ToJSON Fraction where
  toJSON = toJSON . toRational

instance FromJSON Fraction where
  parseJSON = fmap fromRational . parseJSON

-------------------------------------------------
-- OfferAsset
-------------------------------------------------
-- | A wrapper around the offer asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype OfferAsset = OfferAsset { unOfferAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData OfferAsset where
  toBuiltinData (OfferAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData OfferAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap OfferAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData OfferAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    OfferAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to OfferAsset"

instance ToJSON OfferAsset where
  toJSON (OfferAsset (currSym,tokName)) =
    object [ "policy_id" .= display currSym
           , "asset_name" .= display tokName
           ]

instance FromJSON OfferAsset where
  parseJSON = withObject "OfferAsset" $ \o ->
    fmap OfferAsset . (,) 
      <$> (o .: "policy_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
      <*> (o .: "asset_name" >>= maybe mzero (return . TokenName) . parseHex)

instance ToNativeAsset OfferAsset where
  toNativeAsset OfferAsset{unOfferAsset=(sym,name)} = mkNativeAsset sym name

instance FromNativeAsset OfferAsset where
  fromNativeAsset asset = OfferAsset (asset ^. #policyId, asset ^. #tokenName)

makeFieldLabelsNoPrefix ''OfferAsset

-------------------------------------------------
-- AskAsset
-------------------------------------------------
-- | A wrapper around the ask asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype AskAsset = AskAsset { unAskAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData AskAsset where
  toBuiltinData (AskAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData AskAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap AskAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData AskAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    AskAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to AskAsset"

instance ToJSON AskAsset where
  toJSON (AskAsset (currSym, tokName)) =
    object [ "policy_id" .= display currSym
           , "asset_name" .= display tokName
           ]

instance FromJSON AskAsset where
  parseJSON = withObject "AskAsset" $ \o ->
    fmap AskAsset . (,) 
      <$> (o .: "policy_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
      <*> (o .: "asset_name" >>= maybe mzero (return . TokenName) . parseHex)

instance ToNativeAsset AskAsset where
  toNativeAsset AskAsset{unAskAsset=(sym,name)} = mkNativeAsset sym name

instance FromNativeAsset AskAsset where
  fromNativeAsset asset = AskAsset (asset ^. #policyId, asset ^. #tokenName)

makeFieldLabelsNoPrefix ''AskAsset

-------------------------------------------------
-- PremiumAsset
-------------------------------------------------
-- | A wrapper around the premium asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype PremiumAsset = PremiumAsset { unPremiumAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData PremiumAsset where
  toBuiltinData (PremiumAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData PremiumAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap PremiumAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData PremiumAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    PremiumAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to PremiumAsset"

instance ToJSON PremiumAsset where
  toJSON (PremiumAsset (currSym, tokName)) =
    object [ "policy_id" .= display currSym
           , "asset_name" .= display tokName
           ]

instance FromJSON PremiumAsset where
  parseJSON = withObject "PremiumAsset" $ \o ->
    fmap PremiumAsset . (,) 
      <$> (o .: "policy_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
      <*> (o .: "asset_name" >>= maybe mzero (return . TokenName) . parseHex)

instance ToNativeAsset PremiumAsset where
  toNativeAsset PremiumAsset{unPremiumAsset=(sym,name)} = mkNativeAsset sym name

instance FromNativeAsset PremiumAsset where
  fromNativeAsset asset = PremiumAsset (asset ^. #policyId, asset ^. #tokenName)

makeFieldLabelsNoPrefix ''PremiumAsset

-------------------------------------------------
-- ContractId
-------------------------------------------------
-- | A wrapper around the token name for a contract's unique identifier.
newtype ContractId = ContractId { unContractId :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Ord,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ContractId where
  toJSON = toJSON . display

instance FromJSON ContractId where
  parseJSON = withText "ContractId" $ maybe mzero (return . ContractId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''ContractId

-------------------------------------------------
-- TradingPairBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that trading pair.
newtype TradingPairBeacon = TradingPairBeacon { unTradingPairBeacon :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON TradingPairBeacon where
  toJSON = toJSON . display

instance FromJSON TradingPairBeacon where
  parseJSON = withText "TradingPairBeacon" $ maybe mzero (return . TradingPairBeacon . TokenName) . parseHex

makeFieldLabelsNoPrefix ''TradingPairBeacon

-------------------------------------------------
-- OfferBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that offer asset.
newtype OfferBeacon = OfferBeacon { unOfferBeacon :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON OfferBeacon where
  toJSON = toJSON . display

instance FromJSON OfferBeacon where
  parseJSON = withText "OfferBeacon" $ maybe mzero (return . OfferBeacon . TokenName) . parseHex

makeFieldLabelsNoPrefix ''OfferBeacon

-------------------------------------------------
-- AskBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that ask asset.
newtype AskBeacon = AskBeacon { unAskBeacon :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON AskBeacon where
  toJSON = toJSON . display

instance FromJSON AskBeacon where
  parseJSON = withText "AskBeacon" $ maybe mzero (return . AskBeacon . TokenName) . parseHex

makeFieldLabelsNoPrefix ''AskBeacon

-------------------------------------------------
-- PremiumBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that premium asset.
newtype PremiumBeacon = PremiumBeacon { unPremiumBeacon :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON PremiumBeacon where
  toJSON = toJSON . display

instance FromJSON PremiumBeacon where
  parseJSON = withText "PremiumBeacon" $ maybe mzero (return . PremiumBeacon . TokenName) . parseHex

makeFieldLabelsNoPrefix ''PremiumBeacon

-------------------------------------------------
-- Terms
-------------------------------------------------
-- | The terms that an options' writer can vary within the same Proposal UTxO.
data Terms = Terms
  { premium :: Integer
  , strikePrice :: Fraction
  , expiration :: PlutusTime
  } deriving (Show,Eq)

instance ToJSON Terms where
  toJSON Terms{..} =
    object [ "premium" .= premium
           , "strike_price" .= strikePrice
           , "expiration" .= expiration
           ]

instance FromJSON Terms where
  parseJSON = withObject "Terms" $ \o ->
    Terms
      <$> o .: "premium"
      <*> o .: "strike_price"
      <*> o .: "expiration"

makeFieldLabelsNoPrefix ''Terms
PlutusTx.unstableMakeIsData ''Terms
