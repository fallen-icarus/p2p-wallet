{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Plutus
  ( -- * Plutus Addresses
    PlutusAddress

    -- * Plutus Time
  , PlutusTime
  , toPlutusTime
  , fromPlutusTime
  , Slot(..)
  , SlotConfig
  , posixTimeToSlot
  , mainnetSlotConfig
  , testnetSlotConfig

    -- * Plutus Rationals
  , PlutusRational
  , PlutusTx.fromGHC
  , PlutusTx.toGHC

    -- * Re-exports
  , PV1.Address(..)
  , PV1.PubKeyHash(..)
  , PV1.Credential(..)
  , PV1.StakingCredential(..)
  , PV1.ScriptHash(..)
  , BuiltinByteString(..)
  , PV1.toPubKeyHash
  , PV1.TxOutRef(..)
  , PV1.TxId(..)
  , PV1.CurrencySymbol(..)
  , PV1.TokenName(..)
  , PV1.Redeemer(..)
  , PV1.Datum(..)
  , PV1.SerialisedScript
  , PV1.RedeemerHash(..)
  , PV1.DatumHash(..)
  , PV2.OutputDatum(..)
  , PlutusTx.UnsafeFromData(..)
  , PlutusTx.ToData(..)
  , PlutusTx.FromData(..)
  , _NoOutputDatum
  , _OutputDatum
  , _OutputDatumHash

    -- * Parsing
  , parseHex
  , parseTxOutRef

    -- * Script Utils
  , hashScript
  , alwaysSucceedPolicyScript
  , alwaysSucceedPolicyHash
  , toRedeemer
  , toDatum
  , fromDatum
  , fromRedeemer
  , scriptHashToPolicyId
  , policyIdToScriptHash
  , hashRedeemer
  , hashDatum
  , applyArguments
  , getScriptSize

    -- * Serialization
  , decodeData
  , encodeData
  , writeData
  , writeScript
  , parseScriptFromCBOR

    -- * Misc
  , toHexidecimal
  , unBuiltinByteString
  , isPubKeyCredential
  , isScriptCredential
  , unsafeFromData
  , unsafeToBuiltinByteString
  ) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Aeson as Aeson
import Data.Aeson.Types qualified as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import Control.Lens qualified as Lens
import Data.Time.Clock.POSIX qualified as Time

import Servant.API qualified as Servant

import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.Internal (Field(..))

import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V1.Address qualified as PV1
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusLedgerApi.V1.Bytes (LedgerBytes(..),fromHex,encodeByteString)
import qualified PlutusCore.MkPlc as PLC
import qualified UntypedPlutusCore as UPLC
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)
import Plutus.Script.Utils.Scripts qualified as PV2
import Cardano.Api qualified as Api 
import Cardano.Api.Shelley (toPlutusData,fromPlutusData,PlutusScript(..))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Ratio qualified as PlutusTx
import PlutusTx qualified

import P2PWallet.Prelude

-------------------------------------------------
-- Optics
-------------------------------------------------
makePrisms ''PV2.OutputDatum

-------------------------------------------------
-- Plutus Addresses
-------------------------------------------------
-- | A type alias for the address type used as part of smart contracts.
-- Helpful for making type signatures more clear.
type PlutusAddress = PV1.Address

-------------------------------------------------
-- Plutus Time
-------------------------------------------------
-- | A type alias for the time type used as part of smart contracts.
-- Helpful for making type signatures more clear.
type PlutusTime = PV1.POSIXTime

-- | Convert system time to on-chain time. System time is in seconds while plutus time is in
-- milliseconds.
toPlutusTime :: Time.POSIXTime -> PlutusTime
toPlutusTime = PV1.POSIXTime 
             . (* 1000) -- Convert seconds -> milliseconds
             . round

-- | Convert on-chain time to system time. System time is in seconds while plutus time is in
-- milliseconds.
fromPlutusTime :: PlutusTime -> Time.POSIXTime
fromPlutusTime = fromInteger 
               . (`div` 1000) -- Convert milliseconds -> seconds.
               . PV1.getPOSIXTime

-- | Datatype to configure the length (ms) of one slot and the beginning of the
-- first slot.
data SlotConfig = SlotConfig
  { scSlotLength :: !Integer
  -- ^ Length (number of milliseconds) of one slot
  , scSlotZeroTime :: !PV2.POSIXTime
  -- ^ Beginning of slot 0 (in milliseconds)
  } deriving (Eq, Show)

newtype Slot = Slot { unSlot :: Integer }
  deriving (Show)
  deriving newtype (Eq,Ord,Num)

instance Display Slot where
  display (Slot x) = show x

-- -- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
-- slotToBeginPOSIXTime :: SlotConfig -> Slot -> PV2.POSIXTime
-- slotToBeginPOSIXTime SlotConfig{scSlotLength, scSlotZeroTime} (Slot n) =
--   let msAfterBegin = n * scSlotLength
--    in PV2.POSIXTime $ PV2.getPOSIXTime scSlotZeroTime + msAfterBegin

-- | Convert a 'POSIXTime' to 'Slot' given a 'SlotConfig'.
posixTimeToEnclosingSlot :: SlotConfig -> PV2.POSIXTime -> Slot
posixTimeToEnclosingSlot SlotConfig{scSlotLength, scSlotZeroTime} (PV2.POSIXTime t) =
  let timePassed = t - PV2.getPOSIXTime scSlotZeroTime
      slotsPassed = timePassed `div` scSlotLength
   in Slot slotsPassed

-- slotToPOSIXTime :: SlotConfig -> Slot -> PV2.POSIXTime
-- slotToPOSIXTime = slotToBeginPOSIXTime

posixTimeToSlot :: SlotConfig -> PV2.POSIXTime -> Slot
posixTimeToSlot = posixTimeToEnclosingSlot

-- | The preproduction testnet has not always had 1 second slots. Therefore, the default settings
-- for SlotConfig are not usable on the testnet. To fix this, the proper SlotConfig must be
-- normalized to "pretend" that the testnet has always used 1 second slot intervals.
--
-- The normalization is done by taking a slot time and subtracting the slot number from it.
-- For example, slot 56919374 occurred at 1712602574 POSIXTime. So subtracting the slot number 
-- from the time yields the normalized 0 time. The final number needs to be converted to
-- milliseconds.
testnetSlotConfig :: SlotConfig
testnetSlotConfig = SlotConfig 1000 $ PV2.POSIXTime $ (1712603045 - 56919845) * 1000

-- | The mainnet config must also be normalized.
mainnetSlotConfig :: SlotConfig
mainnetSlotConfig = SlotConfig 1000 $ PV2.POSIXTime $ (1712661664 - 121095373) * 1000

-------------------------------------------------
-- Plutus Rational
-------------------------------------------------
-- | A type alias for the plutus version of Rational Helpful for making type signatures more clear.
type PlutusRational = PlutusTx.Rational

-------------------------------------------------
-- Parsing
-------------------------------------------------
-- | Parse a hex encoded Text. This is typically used as a step into parsing Plutus builtins.
parseHex :: Text -> Maybe BuiltinByteString
parseHex t = case fromHex $ encodeUtf8 t of
  Right (LedgerBytes bytes') -> Just bytes'
  Left _ -> Nothing

-- | Parse an output reference formatted as "hash#index".
parseTxOutRef :: Text -> Maybe PV1.TxOutRef
parseTxOutRef s = 
    PV1.TxOutRef 
      <$> (PV1.TxId <$> parseHex txHash) 
      <*> readMaybe (toString $ T.drop 1 index)
  where
    (txHash,index) = T.span (/='#') s

-------------------------------------------------
-- Script Utils
-------------------------------------------------
hashScript :: PV1.SerialisedScript -> PV1.ScriptHash
hashScript = PV1.ScriptHash
           . Builtins.toBuiltin
           . Api.serialiseToRawBytes
           . Api.hashScript
           . Api.PlutusScript Api.PlutusScriptV2 
           . PlutusScriptSerialised

-- | The minting policy script used for test tokens.
alwaysSucceedPolicyScript :: PV1.SerialisedScript
alwaysSucceedPolicyScript = PV2.unScript $ PV2.unMintingPolicyScript alwaysSucceedPolicy

-- | The hash of the minting policy used for test tokens.
alwaysSucceedPolicyHash :: PV1.ScriptHash
alwaysSucceedPolicyHash = hashScript alwaysSucceedPolicyScript

hashDatum :: (PV1.ToData a) => a -> PV1.DatumHash
hashDatum = PV2.datumHash . toDatum

hashRedeemer :: (PV1.ToData a) => a -> PV1.RedeemerHash
hashRedeemer = PV2.redeemerHash . toRedeemer

toRedeemer :: (PV1.ToData a) => a -> PV1.Redeemer
toRedeemer = PV1.Redeemer . PV1.dataToBuiltinData . PV1.toData

fromRedeemer :: (PV1.FromData a) => PV1.Redeemer -> Maybe a
fromRedeemer = PV1.fromBuiltinData . PV1.getRedeemer

toDatum :: (PV1.ToData a) => a -> PV1.Datum
toDatum = PV1.Datum . PV1.dataToBuiltinData . PV1.toData

fromDatum :: (PV1.FromData a) => PV1.Datum -> Maybe a
fromDatum = PV1.fromBuiltinData . PV1.getDatum

scriptHashToPolicyId :: PV1.ScriptHash -> PV1.CurrencySymbol
scriptHashToPolicyId = PV1.CurrencySymbol . PV1.getScriptHash

policyIdToScriptHash :: PV1.CurrencySymbol -> PV1.ScriptHash
policyIdToScriptHash = PV1.ScriptHash . PV1.unCurrencySymbol

-- | Apply extra parameters to a plutus script expecting some.
applyArguments :: PV1.SerialisedScript -> [PV1.Data] -> PV1.SerialisedScript
applyArguments p args =
  let termArgs = fmap (PLC.mkConstant ()) args
      applied t = PLC.mkIterAppNoAnn t termArgs
  in PV1.serialiseUPLC $ Lens.over UPLC.progTerm applied $ PV1.uncheckedDeserialiseUPLC p

getScriptSize :: PV2.SerialisedScript -> Integer
getScriptSize = UPLC.serialisedSize

------------------------------------------------
-- Serialization
------------------------------------------------
toJSONValue :: PV1.ToData a => a -> Aeson.Value
toJSONValue = Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema
            . Api.unsafeHashableScriptData
            . fromPlutusData
            . PV1.toData

-- | Export a plutus script for use with cardano-cli.
writeScript :: FilePath -> PV1.SerialisedScript -> IO (Either (Api.FileError ()) ())
writeScript file script = 
  Api.writeFileTextEnvelope @(Api.PlutusScript Api.PlutusScriptV2) (Api.File file) Nothing $ 
    PlutusScriptSerialised script

-- | Export a datum, redeemer for use with cardano-cli.
writeData :: PV1.ToData a => FilePath -> a -> IO ()
writeData file = LBS.writeFile file . Aeson.encode . toJSONValue

fromCardanoScriptData :: Api.HashableScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . toPlutusData . Api.getScriptData

decodeData :: (PV1.FromData a) => Aeson.Value -> Maybe a
decodeData = either (const Nothing) (PV1.fromBuiltinData . fromCardanoScriptData)
           . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

encodeData :: (PV1.ToData a) => a -> Value
encodeData = toJSONValue

parseScriptFromCBOR :: String -> PV1.SerialisedScript
parseScriptFromCBOR script =
  case Base16.decode $ encodeUtf8 script of
    Left e -> error $ "Failed to decode validator: " <> show e
    Right bytes' -> toShort bytes'

-------------------------------------------------
-- Miscellaneous
-------------------------------------------------
-- | Convert a human-readable text to its hexidecimal equivalent. This is useful for getting
-- token names from users.
toHexidecimal :: Text -> Text
toHexidecimal = encodeByteString . E.encodeUtf8

unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString bs) = bs

isPubKeyCredential :: PV1.Credential -> Bool
isPubKeyCredential (PV1.PubKeyCredential _) = True
isPubKeyCredential _ = False

isScriptCredential :: PV1.Credential -> Bool
isScriptCredential (PV1.ScriptCredential _) = True
isScriptCredential _ = False

unsafeFromData :: (PV1.UnsafeFromData a) => PV1.Data -> a
unsafeFromData = PV1.unsafeFromBuiltinData . PV1.dataToBuiltinData

unsafeToBuiltinByteString :: String -> Builtins.BuiltinByteString
unsafeToBuiltinByteString = PV1.getLedgerBytes . fromRight "" . fromHex . fromString

toPaymentPubKeyHash :: PV1.Credential -> Maybe PV1.PubKeyHash
toPaymentPubKeyHash (PV1.PubKeyCredential k) = Just k
toPaymentPubKeyHash _ = Nothing

toPaymentScriptHash :: PV1.Credential -> Maybe PV1.ScriptHash
toPaymentScriptHash (PV1.ScriptCredential k) = Just k
toPaymentScriptHash _ = Nothing

toStakePubKeyHash :: Maybe PV1.StakingCredential -> Maybe PV1.PubKeyHash
toStakePubKeyHash (Just (PV1.StakingHash (PV1.PubKeyCredential pkh))) = Just pkh
toStakePubKeyHash _ = Nothing

toStakeScriptHash :: Maybe PV1.StakingCredential -> Maybe PV1.ScriptHash
toStakeScriptHash (Just (PV1.StakingHash (PV1.ScriptCredential k))) = Just k
toStakeScriptHash _ = Nothing

-------------------------------------------------
-- Orphans
-------------------------------------------------
instance FromJSON PV1.TxOutRef where
  parseJSON = withText "TxOutRef" (maybe mzero return . parseTxOutRef)

instance ToJSON PV1.TxOutRef where
  toJSON = toJSON . display

instance Display PV1.TxOutRef where
  display PV1.TxOutRef{..} = show txOutRefId <> "#" <> show txOutRefIdx

instance ToJSON PV1.ScriptHash where
  toJSON = toJSON @Text . show

instance FromJSON PV1.ScriptHash where
  parseJSON = withText "ScriptHash" $ maybe mzero (return . PV1.ScriptHash) . parseHex

instance ToJSON PV1.PubKeyHash where
  toJSON = toJSON @Text . show

instance FromJSON PV1.PubKeyHash where
  parseJSON = withText "PubKeyHash" $ maybe mzero (return . PV1.PubKeyHash) . parseHex

instance ToJSON PV1.POSIXTime where
  toJSON = toJSON @Text . show . PV2.getPOSIXTime

instance FromJSON PV1.POSIXTime where
  parseJSON = withText "POSIXTime" $ maybe mzero (return . PV2.POSIXTime) . readMaybe . toString

instance Display PV1.CurrencySymbol where
  display = show

instance Display PV1.TokenName where
  -- The typical show for token names includes an "0x" prefix. Converting it to a `PubKeyHash`
  -- is a simple way to omit the prefix.
  display (PV1.TokenName name) = show $ PV1.PubKeyHash name

instance ToField PV1.CurrencySymbol where
  toField = toField . display

instance FromField PV1.CurrencySymbol where
  fromField (Field (SQLText t) _) = maybe mzero (Ok . PV1.CurrencySymbol) $ parseHex t
  fromField f = returnError ConversionFailed f "need a text"

instance Servant.ToHttpApiData PV1.CurrencySymbol where
  toQueryParam = display

instance ToJSON PV1.CurrencySymbol where
  toJSON = toJSON . display

instance FromJSON PV1.CurrencySymbol where
  parseJSON = withText "CurrencySymbol" $ 
    maybe mzero (return . PV1.CurrencySymbol) . parseHex

instance ToJSON PV1.TokenName where
  toJSON = toJSON . display

instance FromJSON PV1.TokenName where
  parseJSON = withText "TokenName" $ 
    maybe mzero (return . PV1.TokenName) . parseHex

instance Servant.ToHttpApiData PV1.TokenName where
  toQueryParam = display

instance ToField PV1.TokenName where
  toField = toField . display

instance FromField PV1.TokenName where
  fromField (Field (SQLText t) _) = maybe mzero (Ok . PV1.TokenName) $ parseHex t
  fromField f = returnError ConversionFailed f "need a text"

instance ToJSON PV1.Address where
  toJSON (PV1.Address paymentCred mStakeCred) = 
    object [ "payment_pub_key_hash" .= fmap (show @Text) (toPaymentPubKeyHash paymentCred)
           , "payment_script_hash" .= fmap (show @Text) (toPaymentScriptHash paymentCred)
           , "stake_pub_key_hash" .= fmap (show @Text) (toStakePubKeyHash mStakeCred)
           , "stake_script_hash" .= fmap (show @Text) (toStakeScriptHash mStakeCred)
           ]

instance FromJSON PV1.Address where
  parseJSON = withObject "Address" $ \o -> do
    paymentKey <- fmap PV1.PubKeyCredential <$> o .:? "payment_pub_key_hash"
    paymentScript <- fmap PV1.ScriptCredential <$> o .:? "payment_script_hash"
    stakeKey <- fmap PV1.PubKeyCredential <$> o .:? "stake_pub_key_hash"
    stakeScript <- fmap PV1.ScriptCredential <$> o .:? "stake_script_hash"

    paymentCred <- maybe mzero return $ paymentKey <|> paymentScript
    let mStakeCred = fmap PV1.StakingHash $ stakeKey <|> stakeScript

    return $ PV1.Address paymentCred mStakeCred

instance ToJSON PV1.Credential where
  toJSON (PV1.PubKeyCredential kh) = object [ "key_hash" .= show @Text kh]
  toJSON (PV1.ScriptCredential sh) = object [ "script_hash" .= show @Text sh]

instance FromJSON PV2.Credential where
  parseJSON val = maybe mzero pure $ asum
      [ Aeson.parseMaybe parsePubKeyCredential val
      , Aeson.parseMaybe parseScriptCredential val
      ]
    where
      parsePubKeyCredential :: Aeson.Value -> Aeson.Parser PV2.Credential
      parsePubKeyCredential = withObject "PubKeyCredential" $ \o ->
        o .: "key_hash" >>= maybe mzero (pure . PV2.PubKeyCredential . PV2.PubKeyHash) . parseHex

      parseScriptCredential :: Aeson.Value -> Aeson.Parser PV2.Credential
      parseScriptCredential = withObject "ScriptCredential" $ \o ->
        o .: "script_hash" >>= maybe mzero (pure . PV2.ScriptCredential . PV2.ScriptHash) . parseHex

instance ToField PV2.Credential where
  toField = toField . encode

instance FromField PV2.Credential where
  fromField = fmap (decode @PV2.Credential) . fromField @LBS.ByteString >=> maybe mzero Ok

instance Display PV2.Credential where
  display (PV2.PubKeyCredential kh) = show kh
  display (PV2.ScriptCredential sh) = show sh

instance Display PV1.POSIXTime where
  display = show . PV1.getPOSIXTime
