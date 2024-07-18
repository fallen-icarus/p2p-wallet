{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

Types to represent the derivation paths for hardware wallet keys:

@
    m / purpose' / coin_type' / account' / chain / address_index
@

-}
module P2PWallet.Data.Core.Internal.KeyDerivation where

import Data.Aeson qualified as Aeson

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-------------------------------------------------
-- Derivation Type
-------------------------------------------------
-- | The type of derivation to use for the hardware wallet. This currently only applies to trezor.
data DerivationType
  = IcarusDerivation
  | LedgerDerivation
  | TrezorIcarusDerivation
  deriving (Show,Eq,Enum,Read,Ord,Generic,Aeson.ToJSON,Aeson.FromJSON)

instance Display DerivationType where
  display LedgerDerivation = "Ledger"
  display IcarusDerivation = "Icarus"
  display TrezorIcarusDerivation = "Trezor-Icarus"

instance ToField DerivationType where
  toField = toField @Text . show

instance FromField DerivationType where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "failed to parse DerivationType") Ok $ 
      readMaybe $ toString t
  fromField f = returnError ConversionFailed f "need a text"

showDerivationTypeForCLI :: DerivationType -> String
showDerivationTypeForCLI LedgerDerivation = "LEDGER"
showDerivationTypeForCLI IcarusDerivation = "ICARUS"
showDerivationTypeForCLI TrezorIcarusDerivation = "ICARUS_TREZOR"

-- | Used for a dropdown menu.
supportedDerivations :: [DerivationType]
supportedDerivations = enumFrom IcarusDerivation

-------------------------------------------------
-- Account Index
-------------------------------------------------
-- | A type representing the account field in the derivaiton path.
newtype AccountIndex = AccountIndex { unAccountIndex :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField,Read)

makeFieldLabelsNoPrefix ''AccountIndex

toAccountIndex :: Int -> Maybe AccountIndex
toAccountIndex n
  | n >= 0 = Just $ AccountIndex n
  | otherwise = Nothing

-------------------------------------------------
-- Address Index
-------------------------------------------------
-- | A type representing the address_index field in the derivaiton path.
newtype AddressIndex = AddressIndex { unAddressIndex :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,Read)

makeFieldLabelsNoPrefix ''AddressIndex

toAddressIndex :: Int -> Maybe AddressIndex
toAddressIndex n
  | n >= 0 = Just $ AddressIndex n
  | otherwise = Nothing

-------------------------------------------------
-- Derivation Path
-------------------------------------------------
-- | The derivation path used for a hardware wallet key.
data DerivationPath
  -- | Payment keys can increment either the account index or the address index.
  -- The chain index is fixed at `0`.
  = PaymentKeyPath AccountIndex AddressIndex
  -- | Stake keys can increment either the account index or the address index.
  -- The chain index is fixed at `2`.
  | StakeKeyPath AccountIndex AddressIndex
  deriving (Show,Eq,Ord,Read)

makePrisms ''DerivationPath

instance Display DerivationPath where
  display path = let root = "1852H/1815H/%dH/%d/%d" in 
    case path of
      PaymentKeyPath (AccountIndex accIx) (AddressIndex addrIx) -> 
        fromString $ printf root accIx (0 :: Int) addrIx
      StakeKeyPath (AccountIndex accIx) (AddressIndex addrIx) -> 
        fromString $ printf root accIx (2 :: Int) addrIx

instance ToField DerivationPath where
  toField = toField . display

instance FromField DerivationPath where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "not a valid derivation path") Ok  $ parseDerivationPath t
  fromField f = returnError ConversionFailed f "need a text"

instance Aeson.ToJSON DerivationPath where
  toJSON = Aeson.toJSON . display

instance Aeson.FromJSON DerivationPath where
  parseJSON = Aeson.withText "DerivationPath" (maybe mzero return . parseDerivationPath)

parseDerivationPath :: Text -> Maybe DerivationPath
parseDerivationPath t = case words $ replace "/" " " t of
    ["1852H","1815H",accField,"2", addrIx] -> 
      StakeKeyPath 
        <$> parseAccountField accField
        <*> parseAddressIndex addrIx
    ["1852H","1815H",accField,"0", addrIx] -> 
      PaymentKeyPath 
        <$> parseAccountField accField
        <*> parseAddressIndex addrIx
    _ -> Nothing
  where
    parseAccountField :: Text -> Maybe AccountIndex
    parseAccountField accField = case toString accField of
      [accIx,'H'] -> readMaybe @Int (toString [accIx]) >>= toAccountIndex
      _ -> Nothing

    parseAddressIndex :: Text -> Maybe AddressIndex
    parseAddressIndex n = readMaybe @Int (toString n) >>= toAddressIndex

-------------------------------------------------
-- Derivation Info
-------------------------------------------------
-- | A type alias for the full derivation information.
type DerivationInfo = (Maybe DerivationType, DerivationPath)

instance ToField DerivationInfo where
  toField = toField @Text . show . Aeson.encode

instance FromField DerivationInfo where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "failed to parse DerivationInfo") Ok
      (readMaybe (toString t) >>= Aeson.decode)
  fromField f = returnError ConversionFailed f "need a text"

instance Display DerivationInfo where
  display (mDerivationType, path) = unwords
    [ display path
    , "("<> display (fromMaybe LedgerDerivation mDerivationType) <> ")"
    ]
