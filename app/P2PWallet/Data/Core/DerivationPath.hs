{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
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
module P2PWallet.Data.Core.DerivationPath where

import Data.Aeson qualified as Aeson

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-------------------------------------------------
-- Account Index
-------------------------------------------------
-- | A type representing the account field in the derivaiton path.
newtype AccountIndex = AccountIndex { unAccountIndex :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

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
  deriving newtype (Eq,Ord,Num)

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
  deriving (Show,Eq,Ord)

makePrisms ''DerivationPath

instance ToField DerivationPath where
  toField = toField . showDerivationPath

instance FromField DerivationPath where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "not a valid derivation path") Ok  $ readDerivationPath t
  fromField f = returnError ConversionFailed f "need a text"

instance Aeson.ToJSON DerivationPath where
  toJSON = Aeson.toJSON . showDerivationPath

instance Aeson.FromJSON DerivationPath where
  parseJSON = Aeson.withText "DerivationPath" (maybe mzero return . readDerivationPath)

readDerivationPath :: Text -> Maybe DerivationPath
readDerivationPath t = case words $ replace "/" " " t of
    ["1852H","1815H",accField,"2", addrIx] -> 
      StakeKeyPath 
        <$> readAccountField accField
        <*> (AddressIndex <$> readIndex addrIx)
    ["1852H","1815H",accField,"0", addrIx] -> 
      PaymentKeyPath 
        <$> readAccountField accField
        <*> (AddressIndex <$> readIndex addrIx)
    _ -> Nothing
  where
    readAccountField :: Text -> Maybe AccountIndex
    readAccountField accField = case toString accField of
      [accIx,'H'] -> AccountIndex <$> readIndex (toText [accIx])
      _ -> Nothing

    -- All indices must be >= 0.
    readIndex :: Text -> Maybe Int
    readIndex n = do
      i <- readMaybe @Int $ toString n
      guard $ i >= 0
      return i
   
showDerivationPath :: DerivationPath -> Text
showDerivationPath = \case
    PaymentKeyPath (AccountIndex accIx) (AddressIndex addrIx) -> fromString $
      printf root accIx (0 :: Int) addrIx
    StakeKeyPath (AccountIndex accIx) (AddressIndex addrIx) -> fromString $
      printf root accIx (2 :: Int) addrIx
  where
    root = "1852H/1815H/%dH/%d/%d"
