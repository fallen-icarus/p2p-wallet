{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

{-

Types to represent the derivation paths for hardware wallet keys:

@
    m / purpose' / coin_type' / account' / chain / address_index
@

-}
module P2PWallet.Data.Core.DerivationPath where

import Data.Aeson qualified as Aeson
import Data.Scientific (floatingOrInteger)

import P2PWallet.Prelude

-- | A type representing the account field in the derivaiton path.
newtype AccountIndex = AccountIndex Int
  deriving (Show,Eq,Ord,Num)

instance Aeson.ToJSON AccountIndex where
  toJSON (AccountIndex i) = Aeson.toJSON i

instance Aeson.FromJSON AccountIndex where
  parseJSON = 
    Aeson.withScientific "AccountIndex" $ \s ->
      either (const mzero) (pure . AccountIndex) $ floatingOrInteger @Double @Int s

unAccountIndex :: AccountIndex -> Int
unAccountIndex (AccountIndex i) = i

-- | A type representing the address_index field in the derivaiton path.
newtype AddressIndex = AddressIndex Int
  deriving (Show,Eq,Ord,Num)

unAddressIndex :: AddressIndex -> Int
unAddressIndex (AddressIndex i) = i

toAddressIndex :: Int -> Maybe AddressIndex
toAddressIndex n
  | n >= 0 = Just $ AddressIndex n
  | otherwise = Nothing

-- | The derivation path used for a hardware wallet key.
data DerivationPath
  -- | Payment keys can increment either the account index or the address index.
  -- The chain index is fixed at `0`.
  = PaymentKeyPath AccountIndex AddressIndex
  -- | Stake keys can only increment the account index. The chain index is fixed at `2` and 
  -- the address_index is fixed at `0.
  | StakeKeyPath AccountIndex
  deriving (Show,Eq,Ord)

instance Aeson.ToJSON DerivationPath where
  toJSON = Aeson.toJSON . showDerivationPath

instance Aeson.FromJSON DerivationPath where
  parseJSON = Aeson.withText "DerivationPath" (maybe mzero return . readDerivationPath)

readDerivationPath :: Text -> Maybe DerivationPath
readDerivationPath t = case words $ replace "/" " " t of
    ["1852H","1815H",accField,"2","0"] -> StakeKeyPath <$> readAccountField accField
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
    PaymentKeyPath accountIndex addressIndex -> fromString $
      printf root (unAccountIndex accountIndex) (0 :: Int) (unAddressIndex addressIndex)
    StakeKeyPath accountIndex -> fromString $
      printf root (unAccountIndex accountIndex) (2 :: Int) (0 :: Int)
  where
    root = "1852H/1815H/%dH/%d/%d"
