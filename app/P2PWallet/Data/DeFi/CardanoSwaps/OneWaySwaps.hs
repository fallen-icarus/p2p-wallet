{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps where

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
-- | Generate the beacon asset name by hashing offer ++ ask. The policy id for
-- ADA is set to "00".
--
-- > sha2_256 ( offerId ++ offerName ++ askId ++ askName )
genPairBeaconName :: OfferAsset -> AskAsset -> TokenName
genPairBeaconName (OfferAsset assetX) (AskAsset assetY) =
    TokenName $ PlutusTx.sha2_256 $ 
      symbolX <> unTokenName (assetX ^. #tokenName) <> symbolY <> unTokenName (assetY ^. #tokenName)
  where
    symbolX
      | assetX ^. #policyId == "" = unsafeToBuiltinByteString "00"
      | otherwise = unCurrencySymbol $ assetX ^. #policyId
    symbolY
      | assetY ^. #policyId == "" = unsafeToBuiltinByteString "00"
      | otherwise = unCurrencySymbol $ assetY ^. #policyId
  
-- | Generate the beacon asset name by hashing the offer asset policy id and name. 
--
-- > sha2_256 ( "01" ++ offerId ++ offerName )
genOfferBeaconName :: OfferAsset -> TokenName
genOfferBeaconName (OfferAsset NativeAsset{policyId,tokenName}) =
  TokenName $ PlutusTx.sha2_256 $ 
    unsafeToBuiltinByteString "01" <> unCurrencySymbol policyId <> unTokenName tokenName

-- | Generate the beacon asset name by hashing the ask asset policy id and name.
--
-- > sha2_256 ( "02" ++ askId ++ askName )
genAskBeaconName :: AskAsset -> TokenName
genAskBeaconName (AskAsset NativeAsset{policyId,tokenName}) =
  TokenName $ PlutusTx.sha2_256 $ 
    unsafeToBuiltinByteString "02" <> unCurrencySymbol policyId <> unTokenName tokenName

-------------------------------------------------
-- Smart Contracts
-------------------------------------------------
-- | The one-way swap smart contract used for the payment credential in all one-way swap
-- addresses.
swapScript :: SerialisedScript
swapScript = parseScriptFromCBOR $ blueprints Map.! "one_way_swap.swap_script"

-- | The hash of the one-way swap script.
swapScriptHash :: ScriptHash
swapScriptHash = hashScript swapScript

-- | The beacon minting policy for one-way swaps.
beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "one_way_swap.beacon_script")
    [PlutusTx.toData swapScriptHash]

-- | The hash of the one-way swap beacon minting policy.
beaconScriptHash :: ScriptHash
beaconScriptHash = hashScript beaconScript

-- | The policy id for the one-way swap beacon minting policy.
beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptHashToPolicyId beaconScriptHash

-------------------------------------------------
-- Reference Script UTxOs
-------------------------------------------------
-- The reference scripts are locked at the swap address without any staking credential.
-- For testnet, that address is: addr_test1wqql5djxthlrdcnvy87m7uswf0d0es9cdw6nvl72gcqj74s38ksy4.
--
-- The scripts are deliberately stored with an invalid datum so that they are locked forever.

swapScriptTestnetRef :: TxOutRef
swapScriptTestnetRef = 
  fromJust $ parseTxOutRef "9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#0"

beaconScriptTestnetRef :: TxOutRef
beaconScriptTestnetRef = 
  fromJust $ parseTxOutRef "9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#1"

-------------------------------------------------
-- Swap Datum
-------------------------------------------------
data SwapDatum = SwapDatum
  -- | `CurrencySymbol` for the `beaconScript`.
  { beaconId :: CurrencySymbol
  -- | The pair beacon's `TokenName` for this trading pair.
  , pairBeacon :: TokenName
  -- | The `CurrencySymbol` for the offer asset.
  , offerId :: CurrencySymbol
  -- | The `TokenName` for the offer asset.
  , offerName :: TokenName
  -- | The offer beacon's `TokenName`.
  , offerBeacon :: TokenName
  -- | The `CurrencySymbol` for the ask asset.
  , askId :: CurrencySymbol
  -- | The `TokenName` for the ask asset.
  , askName :: TokenName
  -- | The ask beacon's `TokenName`.
  , askBeacon :: TokenName
  -- | The price to take the offer asset as a fraction (Ask/Offer). 
  , swapPrice :: PlutusRational
  -- | The corresponding swap input's output reference.
  , prevInput :: Maybe TxOutRef
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapDatum

instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= display beaconId
           , "pair_beacon" .= display pairBeacon
           , "offer_id" .= display offerId
           , "offer_name" .= display offerName
           , "offer_beacon" .= display offerBeacon
           , "ask_id" .= display askId
           , "ask_name" .= display askName
           , "ask_beacon" .= display askBeacon
           , "price" .= swapPrice
           , "prev_input" .= prevInput
           ]

instance FromJSON SwapDatum where
  parseJSON = 
    withObject "OneWaySwapDatum" $ \o ->
      SwapDatum
        <$> (o .: "beacon_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "pair_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "offer_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "offer_name" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "offer_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "ask_id" >>= maybe mzero (return . CurrencySymbol) . parseHex)
        <*> (o .: "ask_name" >>= maybe mzero (return . TokenName) . parseHex)
        <*> (o .: "ask_beacon" >>= maybe mzero (return . TokenName) . parseHex)
        <*> o .: "price"
        <*> o .: "prev_input"

-- | Create the datum for a one-way swap.
genSwapDatum :: OfferAsset -> AskAsset -> Rational -> Maybe TxOutRef -> SwapDatum
genSwapDatum offerAsset askAsset price mPrev = SwapDatum 
  { beaconId = beaconCurrencySymbol
  , pairBeacon = genPairBeaconName offerAsset askAsset
  , offerId = offerAsset ^. #unOfferAsset % #policyId
  , offerName = offerAsset ^. #unOfferAsset % #tokenName
  , offerBeacon = genOfferBeaconName offerAsset
  , askId = askAsset ^. #unAskAsset % #policyId
  , askName = askAsset ^. #unAskAsset % #tokenName
  , askBeacon = genAskBeaconName askAsset
  , swapPrice = fromGHC price
  , prevInput = mPrev
  }

-------------------------------------------------
-- Swap Redeemer
-------------------------------------------------
data SwapRedeemer
  -- | Spend the swap as the owner using the beacon script as a minting policy.
  = SpendWithMint
  -- | Spend the swap as the owner using the beacon script as a staking validator. This is only
  -- for when no beacons need to be minted or burned in the transaction.
  | SpendWithStake
  -- | Take the offer asset and deposit the ask asset.
  | Swap
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

-------------------------------------------------
-- Swap Address
-------------------------------------------------
-- | Create the swap address for the stake credential. The credential is the staking credential
-- of the new swap address.
genSwapAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genSwapAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential swapScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a one-way swap address.
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
