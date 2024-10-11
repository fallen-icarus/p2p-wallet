{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoAftermarket.Internal 
  ( BeaconId(..)
  , PolicyBeacon(..)
  , BidderId(..)
  , Asset(..)
  , Prices(..)
  ) where

import Data.Aeson

import qualified PlutusLedgerApi.V2 as PV2

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- BeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the beacon script.
newtype BeaconId = BeaconId { unBeaconId :: PV2.CurrencySymbol }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON BeaconId where
  toJSON = toJSON . display

instance FromJSON BeaconId where
  parseJSON = withText "BeaconId" $ 
    maybe mzero (return . BeaconId . CurrencySymbol) . parseHex

makeFieldLabelsNoPrefix ''BeaconId

-------------------------------------------------
-- PolicyBeacon
-------------------------------------------------
-- | A wrapper around the token name for the policy beacon of the NFTs being sold.
newtype PolicyBeacon = PolicyBeacon { unPolicyBeacon :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON PolicyBeacon where
  toJSON = toJSON . display

instance FromJSON PolicyBeacon where
  parseJSON = withText "PolicyBeacon" $ 
    maybe mzero (return . PolicyBeacon . TokenName) . parseHex

makeFieldLabelsNoPrefix ''PolicyBeacon

-------------------------------------------------
-- BidderId
-------------------------------------------------
-- | A wrapper around the token name for the bidder id beacon.
newtype BidderId = BidderId { unBidderId :: PV2.TokenName }
  deriving (Show)
  deriving newtype (IsString,Display,Eq,PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON BidderId where
  toJSON = toJSON . display

instance FromJSON BidderId where
  parseJSON = withText "BidderId" $ 
    maybe mzero (return . BidderId . TokenName) . parseHex

makeFieldLabelsNoPrefix ''BidderId

-------------------------------------------------
-- Asset
-------------------------------------------------
-- | A wrapper around the asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Asset = Asset { unAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show)
  deriving newtype (Eq,Ord)

instance PV2.ToData Asset where
  toBuiltinData (Asset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData Asset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap Asset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Asset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    Asset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to Asset"

instance ToJSON Asset where
  toJSON (Asset (currSym, tokName)) =
    object [ "policy_id" .= currSym
           , "token_name" .= tokName
           ]

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o ->
    fmap Asset $ (,)
      <$> (o .: "policy_id")
      <*> (o .: "token_name")

makeFieldLabelsNoPrefix ''Asset

-------------------------------------------------
-- Prices
-------------------------------------------------
-- | A wrapper around a list of prices. It uses a custom data encoding since Aiken uses a 
-- different encoding for it.
newtype Prices = Prices { unPrices :: [(Asset,Integer)] }
  deriving (Show)
  deriving newtype (Eq,Ord)

instance PV2.ToData Prices where
  toBuiltinData (Prices xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData) xs

instance PV2.FromData Prices where
  fromBuiltinData (PV2.BuiltinData (PV2.Map xs)) = 
    fmap Prices $ sequence $ 
        flip map xs $ \(x,y) -> (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Prices where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map xs)) = 
    Prices $ map (bimap unsafeFromData unsafeFromData) xs
  unsafeFromBuiltinData _ = error "Could not convert Data to Prices"

instance ToJSON Prices where
  toJSON (Prices xs) = object [ "prices" .= map toJSON xs ]

instance FromJSON Prices where
  parseJSON = withObject "Prices" $ fmap Prices . (.: "prices")

instance ToNativeAsset (Asset,Integer) where
  toNativeAsset (Asset{unAsset=(sym,name)},num) = 
    mkNativeAsset sym name & #quantity .~ num

instance FromNativeAsset (Asset,Integer) where
  fromNativeAsset asset = (Asset (asset ^. #policyId, asset ^. #tokenName), asset ^. #quantity)

makeFieldLabelsNoPrefix ''Prices
