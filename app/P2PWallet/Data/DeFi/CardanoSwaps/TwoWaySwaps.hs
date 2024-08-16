{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps where

import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Maybe (fromJust)

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

import CardanoSwaps.Blueprints

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Generate the beacon asset name by hashing asset1 ++ asset2. The trading pair is first
-- sorted so that the beacon name is independent of the ordering. The policy id for
-- ADA is set to "00" __after__ sorting.
--
-- > sha2_256 ( asset1Id ++ asset1Name ++ asset2Id ++ asset2Name )
genPairBeaconName :: NativeAsset -> NativeAsset -> TokenName
genPairBeaconName assetX assetY =
    TokenName $ PlutusTx.sha2_256 $ 
      symbol1 <> unTokenName (asset1 ^. #tokenName) <> symbol2 <> unTokenName (asset2 ^. #tokenName)
  where
    (asset1,asset2) = if assetY < assetX then (assetY,assetX) else (assetX,assetY) 
    symbol1
      | asset1 ^. #policyId == "" = unsafeToBuiltinByteString "00"
      | otherwise = unCurrencySymbol $ asset1 ^. #policyId
    symbol2
      | asset2 ^. #policyId == "" = unsafeToBuiltinByteString "00"
      | otherwise = unCurrencySymbol $ asset2 ^. #policyId

-- | Generate the beacon asset name by hashing the asset policy id and name. 
--
-- > sha2_256 ( assetId ++ assetName )
genAssetBeaconName :: NativeAsset -> TokenName
genAssetBeaconName NativeAsset{policyId,tokenName} =
  TokenName $ PlutusTx.sha2_256 $ unCurrencySymbol policyId <> unTokenName tokenName

-------------------------------------------------
-- Smart Contracts
-------------------------------------------------
-- | The two-way swap smart contract used for the payment credential in all two-way swap
-- addresses.
swapScript :: SerialisedScript
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.swap_script"

-- | The hash of the two-way swap script.
swapScriptHash :: ScriptHash
swapScriptHash = hashScript swapScript

-- | The beacon minting policy for two-way swaps.
beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.beacon_script")
    [PlutusTx.toData swapScriptHash]

-- | The hash of the two-way swap beacon minting policy.
beaconScriptHash :: ScriptHash
beaconScriptHash = hashScript beaconScript

-- | The policy id for the two-way swap beacon minting policy.
beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptHashToPolicyId beaconScriptHash

-------------------------------------------------
-- Reference Script UTxOs
-------------------------------------------------
-- The reference scripts are locked at the swap address without any staking credential.
-- For testnet, that address is: addr_test1wzrns8ct7stw9kh8f97nlnvqsl8kw7eukje2aw3kak8c77g25nluj.
--
-- The scripts are deliberately stored with an invalid datum so that they are locked forever.

swapScriptTestnetRef :: TxOutRef
swapScriptTestnetRef = 
  fromJust $ parseTxOutRef "115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#0"

beaconScriptTestnetRef :: TxOutRef
beaconScriptTestnetRef = 
  fromJust $ parseTxOutRef "115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#1"

-------------------------------------------------
-- Swap Datum
-------------------------------------------------
data SwapDatum = SwapDatum
  -- | `CurrencySymbol` for the `beaconScript`.
  { beaconId :: CurrencySymbol
  -- | The pair beacon's `TokenName` for this trading pair.
  , pairBeacon :: TokenName
  -- | The `CurrencySymbol` for asset1.
  , asset1Id :: CurrencySymbol
  -- | The `TokenName` for asset1.
  , asset1Name :: TokenName
  -- | The asset beacon's `TokenName` for asset1.
  , asset1Beacon :: TokenName
  -- | The `CurrencySymbol` for asset2.
  , asset2Id :: CurrencySymbol
  -- | The `TokenName` for asset2.
  , asset2Name :: TokenName
  -- | The asset beacon's `TokenName` for asset2.
  , asset2Beacon :: TokenName
  -- | The price to take asset1 as a fraction (Asset2/Asset1). 
  , asset1Price :: PlutusRational
  -- | The price to take asset2 as a fraction (Asset1/Asset2). 
  , asset2Price :: PlutusRational
  -- | The corresponding swap input's output reference.
  , prevInput :: Maybe TxOutRef
  } deriving (Generic,Show,Eq)

makeFieldLabelsNoPrefix ''SwapDatum

instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= display beaconId
           , "pair_beacon" .= display pairBeacon
           , "asset1_id" .= display asset1Id
           , "asset1_name" .= display asset1Name
           , "asset1_beacon" .= display asset1Beacon
           , "asset2_id" .= display asset2Id
           , "asset2_name" .= display asset2Name
           , "asset2_beacon" .= display asset2Beacon
           , "asset1_price" .= asset1Price
           , "asset2_price" .= asset2Price
           , "prev_input" .= prevInput
           ]

instance FromJSON SwapDatum where
  parseJSON = 
    withObject "TwoWaySwapDatum" $ \o ->
      SwapDatum
        <$> (o .: "beacon_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "pair_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "asset1_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "asset1_name" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "asset1_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "asset2_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "asset2_name" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "asset2_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> o .: "asset1_price"
        <*> o .: "asset2_price"
        <*> o .: "prev_input"

-- | Create the datum for a two-swap. The native assets should be paired with the price for _taking_
-- that asset (ie, other_asset/this_asset). Which asset is first or second does not matter; this
-- function will create the datum in the proper order.
genSwapDatum :: (NativeAsset,Rational) -> (NativeAsset,Rational) -> Maybe TxOutRef -> SwapDatum
genSwapDatum (firstAsset,firstPrice) (secondAsset,secondPrice) mPrev = SwapDatum 
    { beaconId = beaconCurrencySymbol
    , pairBeacon = genPairBeaconName asset1 asset2
    , asset1Id = asset1 ^. #policyId
    , asset1Name = asset1 ^. #tokenName
    , asset1Beacon = genAssetBeaconName asset1
    , asset2Id = asset2 ^. #policyId
    , asset2Name = asset2 ^. #tokenName
    , asset2Beacon = genAssetBeaconName asset2
    , asset1Price = fromGHC asset1Price
    , asset2Price = fromGHC asset2Price
    , prevInput = mPrev
    }
  where
    ((asset1,asset1Price), (asset2,asset2Price))
      | firstAsset < secondAsset = ((firstAsset,firstPrice), (secondAsset, secondPrice)) 
      | otherwise = ((secondAsset,secondPrice), (firstAsset,firstPrice))

-------------------------------------------------
-- Swap Redeemers
-------------------------------------------------
data SwapRedeemer
  -- | Spend the swap as the owner using the beacon script as a minting policy.
  = SpendWithMint 
  -- | Spend the swap as the owner using the beacon script as a staking validator. This is only
  -- for when no beacons need to be minted or burned in the transaction.
  | SpendWithStake 
  -- | Take asset1 and deposit asset2.
  | TakeAsset1 
  -- | Take asset2 and deposit asset1.
  | TakeAsset2 
  deriving (Eq,Generic,Show)

data BeaconRedeemer
  -- | Execute the beacon script as a minting policy. Used anytime beacons must be minted or burned.
  = CreateOrCloseSwaps 
  -- | Execute the beacon script as a staking validtor. Used anytime beacons do not need to be
  -- minted or burned.
  | UpdateSwaps
  deriving (Eq,Generic,Show)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-- | Get the required two-way swap redeemer based on the desired swap direction.
getRequiredSwapDirection :: OfferAsset -> AskAsset -> SwapRedeemer
getRequiredSwapDirection (OfferAsset offerAsset) (AskAsset askAsset)
  | offerAsset == min offerAsset askAsset = TakeAsset1
  | otherwise = TakeAsset2

-------------------------------------------------
-- Swap Address
-------------------------------------------------
-- | Create the swap address for the stake credential. The credential is the staking credential for
-- the new swap address.
genSwapAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genSwapAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential swapScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a two-way swap address.
isSwapAddress :: PaymentAddress -> Bool
isSwapAddress = either (const False) ((== ScriptCredential swapScriptHash) . addressCredential)
              . paymentAddressToPlutusAddress

-- | The address that must be used for the beacon staking execution.
beaconStakeAddress :: Network -> StakeAddress
beaconStakeAddress network = fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
  Address
    { addressCredential = ScriptCredential beaconScriptHash
    , addressStakingCredential = Just $ StakingHash $ ScriptCredential beaconScriptHash
    }
