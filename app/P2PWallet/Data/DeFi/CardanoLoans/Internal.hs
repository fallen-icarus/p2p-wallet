{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoLoans.Internal 
  ( NegotiationBeaconId(..)
  , ActiveBeaconId(..)
  , LenderId(..)
  , BorrowerId(..)
  , AssetBeaconId(..)
  , LoanId(..)
  , Fraction(..)
  , Penalty(..)
  , Asset(..)
  , Collateralization(..)
  , Collateral(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Ratio qualified as Ratio

import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

import qualified PlutusTx
import qualified PlutusLedgerApi.V2 as PV2

import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Negotiation Phase Beacon ID
-------------------------------------------------
-- | A wrapper around the policy id for the negotation beacon script.
newtype NegotiationBeaconId = NegotiationBeaconId { unNegotiationBeaconId :: CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (Display,ToData,FromData,UnsafeFromData,IsString)

instance ToJSON NegotiationBeaconId where
  toJSON = toJSON . display

instance FromJSON NegotiationBeaconId where
  parseJSON = withText "NegotiationBeaconId" $ 
    maybe mzero (return . NegotiationBeaconId . CurrencySymbol) . parseHex

makeFieldLabelsNoPrefix ''NegotiationBeaconId

-------------------------------------------------
-- Active Phase Beacon ID
-------------------------------------------------
-- | A wrapper around the policy id for the active beacon script.
newtype ActiveBeaconId = ActiveBeaconId { unActiveBeaconId :: CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (Display,ToData,FromData,UnsafeFromData,IsString)

instance ToJSON ActiveBeaconId where
  toJSON = toJSON . display

instance FromJSON ActiveBeaconId where
  parseJSON = withText "ActiveBeaconId" $ 
    maybe mzero (return . ActiveBeaconId . CurrencySymbol) . parseHex

makeFieldLabelsNoPrefix ''ActiveBeaconId

-------------------------------------------------
-- Lender Beacon ID
-------------------------------------------------
-- | A wrapper around the token name for a lender id. It is prefixed with
-- either "00" or "01" depending on whether the lender's credential is a pub key credential
-- or a script credential, respectively.
newtype LenderId = LenderId { unLenderId :: TokenName }
  deriving (Show,Eq)
  deriving newtype (Display,ToData,FromData,UnsafeFromData,ToField,FromField,IsString)

instance ToJSON LenderId where
  toJSON = toJSON . display

instance FromJSON LenderId where
  parseJSON = withText "LenderId" $ 
    maybe mzero (return . LenderId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''LenderId

-------------------------------------------------
-- Borrower Beacon ID
-------------------------------------------------
-- | A wrapper around the token name for a borrower id.
newtype BorrowerId = BorrowerId { unBorrowerId :: TokenName }
  deriving (Show,Eq)
  deriving newtype (Display,ToData,FromData,UnsafeFromData,ToField,FromField,IsString)

instance ToJSON BorrowerId where
  toJSON = toJSON . display

instance FromJSON BorrowerId where
  parseJSON = withText "BorrowerId" $ 
    maybe mzero (return . BorrowerId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''BorrowerId

-------------------------------------------------
-- Asset Beacon ID
-------------------------------------------------
-- | A wrapper around the token name for a loan asset's beacon name. The name is:
-- sha2_256 ( "Asset" ++ policy id ++ token name ).
newtype AssetBeaconId = AssetBeaconId { unAssetBeaconId :: TokenName }
  deriving (Show,Eq)
  deriving newtype (Display,ToData,FromData,UnsafeFromData,IsString)

instance ToJSON AssetBeaconId where
  toJSON = toJSON . display

instance FromJSON AssetBeaconId where
  parseJSON = withText "AssetBeaconId" $ 
    maybe mzero (return . AssetBeaconId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''AssetBeaconId

-------------------------------------------------
-- Loan ID
-------------------------------------------------
-- | A wrapper around the token name for a loan's unique identifier. The name is:
-- sha2_256 ( offer tx hash ++ offer output index ).
newtype LoanId = LoanId { unLoanId :: TokenName }
  deriving (Show)
  deriving newtype (Eq,Ord,Display,ToData,FromData,UnsafeFromData,IsString)

instance ToJSON LoanId where
  toJSON = toJSON . display

instance FromJSON LoanId where
  parseJSON = withText "LoanId" $ 
    maybe mzero (return . LoanId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''LoanId

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
-- Penalty
-------------------------------------------------
-- | The penalty to apply whenever the minimum payment is not met.
data Penalty
  = NoPenalty
  | FixedFee Integer
  | PercentFee Fraction
  deriving (Show,Eq)

instance ToJSON Penalty where
  toJSON NoPenalty = "none"
  toJSON (FixedFee fee) = object [ "fixed_fee" .= fee ]
  toJSON (PercentFee fee) = object [ "percent_fee" .= toJSON fee ]

instance FromJSON Penalty where
  parseJSON val = maybe mzero return $ asum
      [ parseNone
      , parseFixed
      , parsePercent
      ]
    where
      parseNone :: Maybe Penalty
      parseNone = 
        parseMaybe (withText "NoPenalty" $ \t -> if t == "none" then return NoPenalty else mzero) val

      parseFixed :: Maybe Penalty
      parseFixed = 
        parseMaybe (withObject "FixedFee" (fmap FixedFee . (.: "fixed_fee"))) val

      parsePercent :: Maybe Penalty
      parsePercent = 
        parseMaybe (withObject "PercentFee" (fmap PercentFee . (.: "percent_fee"))) val

PlutusTx.unstableMakeIsData ''Penalty

-------------------------------------------------
-- Asset
-------------------------------------------------
-- | A wrapper around an asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Asset = Asset { unAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance ToData Asset where
  toBuiltinData (Asset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance FromData Asset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap Asset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance UnsafeFromData Asset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    Asset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to Asset"

instance ToJSON Asset where
  toJSON (Asset (currSym, tokName)) =
    object [ "policy_id" .= display currSym
           , "token_name" .= display tokName
           ]

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o ->
    fmap Asset $ (,)
      <$> (o .: "policy_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
      <*> (o .: "token_name" >>= maybe mzero (return . TokenName) . parseHex)

instance ToNativeAsset Asset where
  toNativeAsset Asset{unAsset=(sym,name)} = mkNativeAsset sym name

instance FromNativeAsset Asset where
  fromNativeAsset asset = Asset (asset ^. #policyId, asset ^. #tokenName)

-------------------------------------------------
-- Collateralization
-------------------------------------------------
-- | A wrapper around a list of collateral and their values relative to the loan asset. It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Collateralization = Collateralization { unCollateralization :: [(Asset,Fraction)] }
  deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Collateralization

instance ToData Collateralization where
  toBuiltinData (Collateralization xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData) xs

instance FromData Collateralization where
  fromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    fmap Collateralization $ sequence $ 
        flip map collats $ \(x,y) -> (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance UnsafeFromData Collateralization where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    Collateralization $ map (bimap unsafeFromData unsafeFromData) collats
  unsafeFromBuiltinData _ = error "Could not convert Data to Collateralization"

instance ToJSON Collateralization where
  toJSON (Collateralization xs) = object [ "collateralization" .= xs ]

instance FromJSON Collateralization where
  parseJSON = withObject "Collateralization" $ fmap Collateralization . (.: "collateralization")

-------------------------------------------------
-- Collateral
-------------------------------------------------
-- | A wrapper around a list of collateral. It uses a custom data encoding since Aiken uses a 
-- different encoding for it.
newtype Collateral = Collateral { unCollateral :: [Asset] }
  deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Collateral

instance ToData Collateral where
  toBuiltinData (Collateral xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData . unAsset) xs

instance FromData Collateral where
  fromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    fmap Collateral $ sequence $ 
        flip map collats $ \(x,y) -> fmap Asset . (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance UnsafeFromData Collateral where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    Collateral $ map (Asset . bimap unsafeFromData unsafeFromData) collats
  unsafeFromBuiltinData _ = error "Could not convert Data to Collateral"

instance ToJSON Collateral where
  toJSON (Collateral xs) = object [ "collateral" .= xs ]

instance FromJSON Collateral where
  parseJSON = withObject "Collateral" $ fmap Collateral . (.: "collateral")
