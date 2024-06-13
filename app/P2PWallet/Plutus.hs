{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

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

    -- * Asset Fingerprints
  , mkAssetFingerprint

    -- * Parsing
  , readHex
  , readTxOutRef
  , readPubKeyHash

    -- * Misc
  , unBuiltinByteString
  , showTxOutRef
  , isPubKeyCredential
  ) where

import Data.Text qualified as T
import Data.Aeson
import qualified Codec.Binary.Bech32 as Bech32
import Codec.Binary.Bech32.TH (humanReadablePart)
import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (Blake2b_160)
import Data.ByteArray (convert)

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
readHex :: Text -> Maybe BuiltinByteString
readHex t = case fromHex $ encodeUtf8 t of
  Right (LedgerBytes bytes') -> Just bytes'
  Left _ -> Nothing

readTxOutRef :: Text -> Maybe PV1.TxOutRef
readTxOutRef s = 
    PV1.TxOutRef 
      <$> (PV1.TxId <$> readHex txHash) 
      <*> readMaybe (toString $ T.drop 1 index)
  where
    (txHash,index) = T.span (/='#') s

-- | Parse PubKeyHash from user supplied Text.
readPubKeyHash :: Text -> Maybe PV1.PubKeyHash
readPubKeyHash = fmap PV1.PubKeyHash . readHex

-------------------------------------------------
-- Asset Fingerprints
-------------------------------------------------
mkAssetFingerprint :: Text -> Text -> Either Text Text
mkAssetFingerprint policyAsText tokenAsText = do
    BuiltinByteString policyAsByte <- maybeToRight "Not a valid policy id" $ readHex policyAsText
    BuiltinByteString tokenAsByte <- maybeToRight "Not a valid token name" $ readHex tokenAsText

    return $ Bech32.encodeLenient hrp 
           $ Bech32.dataPartFromBytes 
           $ convert 
           $ hash @_ @Blake2b_160 
           $ policyAsByte <> tokenAsByte
  where
    hrp = [humanReadablePart|asset|]

-------------------------------------------------
-- Miscellaneous
-------------------------------------------------
unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString bs) = bs

showTxOutRef :: forall a. (IsString a, Semigroup a) => PV1.TxOutRef -> a
showTxOutRef PV1.TxOutRef{..} = show @a txOutRefId <> "#" <> show @a txOutRefIdx

isPubKeyCredential :: PV1.Credential -> Bool
isPubKeyCredential (PV1.PubKeyCredential _) = True
isPubKeyCredential _ = False

-------------------------------------------------
-- Orphans
-------------------------------------------------
instance FromJSON PV1.TxOutRef where
  parseJSON = withText "TxOutRef" (maybe mzero return . readTxOutRef)

instance ToJSON PV1.TxOutRef where
  toJSON = toJSON . showTxOutRef @String
