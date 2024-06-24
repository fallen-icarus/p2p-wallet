{-# OPTIONS_GHC -Wno-orphans #-}

module P2PWallet.Plutus
  ( -- * Plutus Addresses
    PlutusAddress

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

    -- * Parsing
  , parseHex
  , parseTxOutRef
  , parsePubKeyHash

    -- * Misc
  , unBuiltinByteString
  , isPubKeyCredential
  ) where

import Data.Text qualified as T
import Data.Aeson

import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(..), returnError, ResultError(ConversionFailed))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.Internal (Field(..))

import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Address qualified as PV1
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusLedgerApi.V1.Bytes (LedgerBytes(..),fromHex)

import P2PWallet.Prelude

-------------------------------------------------
-- Plutus Addresses
-------------------------------------------------
-- | A type alias for the address type used as part of smart contracts.
-- Helpful for making type signatures more clear.
type PlutusAddress = PV1.Address

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

-- | Parse PubKeyHash from user supplied Text.
parsePubKeyHash :: Text -> Maybe PV1.PubKeyHash
parsePubKeyHash = fmap PV1.PubKeyHash . parseHex

-------------------------------------------------
-- Miscellaneous
-------------------------------------------------
unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString bs) = bs

isPubKeyCredential :: PV1.Credential -> Bool
isPubKeyCredential (PV1.PubKeyCredential _) = True
isPubKeyCredential _ = False

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
