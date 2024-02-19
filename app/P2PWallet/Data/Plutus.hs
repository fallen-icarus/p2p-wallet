{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Data.Plutus
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
-- Miscellaneous
-------------------------------------------------
unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString bs) = bs

showTxOutRef :: forall a. (IsString a, Semigroup a) => PV1.TxOutRef -> a
showTxOutRef PV1.TxOutRef{..} = show @a txOutRefId <> "#" <> show @a txOutRefIdx

isPubKeyCredential :: PV1.Credential -> Bool
isPubKeyCredential (PV1.PubKeyCredential _) = True
isPubKeyCredential _ = False
