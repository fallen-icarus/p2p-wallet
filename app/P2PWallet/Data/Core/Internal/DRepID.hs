{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.DRepID where

import Data.Aeson 
import Codec.Binary.Encoding qualified as E
import Data.Text.Encoding qualified as T
import Data.ByteString qualified as BS
import Cardano.Codec.Bech32.Prefixes qualified as Cardano

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import P2PWallet.Plutus
import P2PWallet.Prelude

newtype DRepID = DRepID { unDRepId :: Text }
  deriving (Show)
  deriving newtype (Eq,Ord,ToField,FromField,FromJSON,ToJSON,IsString,ToText,ToString)

makeFieldLabelsNoPrefix ''DRepID

instance Display DRepID where
  -- Koios prepends `22` onto the credential hash before bech32 encoding it. Since users are expecting
  -- the bech32 encoding of the credential hash without the `22`, it must be dropped.
  display drepId = fromRight "Invalid DRepID" $ drepIdToHex drepId >>= fmap unDRepId . hexToDrepId

drepIdToHex :: DRepID -> Either Text Text
drepIdToHex (DRepID drepId) = do
  res <- bimap toText snd $ E.fromBech32 (const id) $ T.encodeUtf8 drepId
  case BS.length res of
    -- This is a CIP-129 prefixed bech32 encoding.
    29 -> return $ show $ PubKeyHash $ BuiltinByteString $ BS.tail res
    -- This should be 28; an unprefixed DRep credential hash in hexidecimal.
    _ -> return $ show $ PubKeyHash $ BuiltinByteString res

hexToDrepId :: Text -> Either Text DRepID
hexToDrepId h = 
  fmap DRepID $ maybeToRight ("Couldn't parse hexidecimal: '" <> h <> "'") $ 
    T.decodeLatin1 . E.encode (E.EBech32 Cardano.drep) . unBuiltinByteString <$> parseHex h
