{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Plutus
  ( -- * Plutus Addresses
    PlutusAddress

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
  , PV1.CurrencySymbol(..)
  , PV1.TokenName(..)
  , PV1.Redeemer(..)
  , PV1.Datum(..)
  , PV1.SerialisedScript
  , PV1.RedeemerHash(..)
  , PV1.DatumHash(..)
  , PV2.OutputDatum(..)
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
  , scriptHashToPolicyId
  , policyIdToScriptHash
  , hashRedeemer
  , hashDatum
  , applyArguments

    -- * Serialization
  , decodeDatum
  , writeData
  , writeScript
  , parseScriptFromCBOR

    -- * Misc
  , toHexidecimal
  , unBuiltinByteString
  , isPubKeyCredential
  , unsafeFromData
  , unsafeToBuiltinByteString
  ) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import Control.Lens qualified as Lens

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

toDatum :: (PV1.ToData a) => a -> PV1.Datum
toDatum = PV1.Datum . PV1.dataToBuiltinData . PV1.toData

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

decodeDatum :: (PV1.FromData a) => Aeson.Value -> Maybe a
decodeDatum = either (const Nothing) (PV1.fromBuiltinData . fromCardanoScriptData)
            . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

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

unsafeFromData :: (PV1.UnsafeFromData a) => PV1.Data -> a
unsafeFromData = PV1.unsafeFromBuiltinData . PV1.dataToBuiltinData

unsafeToBuiltinByteString :: String -> Builtins.BuiltinByteString
unsafeToBuiltinByteString = PV1.getLedgerBytes . fromRight "" . fromHex . fromString

-------------------------------------------------
-- Orphans
-------------------------------------------------
instance FromJSON PV1.TxOutRef where
  parseJSON = withText "TxOutRef" (maybe mzero return . parseTxOutRef)

instance ToJSON PV1.TxOutRef where
  toJSON = toJSON . display

instance Display PV1.TxOutRef where
  display PV1.TxOutRef{..} = show txOutRefId <> "#" <> show txOutRefIdx

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

instance ToField PV1.TokenName where
  toField = toField . display

instance FromField PV1.TokenName where
  fromField (Field (SQLText t) _) = maybe mzero (Ok . PV1.TokenName) $ parseHex t
  fromField f = returnError ConversionFailed f "need a text"
