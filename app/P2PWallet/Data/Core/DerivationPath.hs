{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.DerivationPath where

import Data.Aeson qualified as Aeson

import P2PWallet.Prelude

-- | The derivation path used for a hardware wallet key.
data DerivationPath
  = PaymentKeyPath Int
  | StakeKeyPath Int
  deriving (Show,Eq,Ord)

instance Aeson.ToJSON DerivationPath where
  toJSON = Aeson.toJSON . showDerivationPath

instance Aeson.FromJSON DerivationPath where
  parseJSON = Aeson.withText "DerivationPath" (maybe mzero return . readDerivationPath)

readDerivationPath :: Text -> Maybe DerivationPath
readDerivationPath t = case words $ replace "/" " " t of
    ["1852H","1815H","0H","2",n] -> StakeKeyPath <$> readIndex n
    ["1852H","1815H","0H","0", n] -> PaymentKeyPath <$> readIndex n
    _ -> Nothing
  where
    readIndex :: Text -> Maybe Int
    readIndex n = do
      i <- readMaybe @Int $ toString n
      guard $ i >= 0
      return i
   
showDerivationPath :: DerivationPath -> Text
showDerivationPath = \case
    PaymentKeyPath i -> root <> "0/" <> show i
    StakeKeyPath i -> root <> "2/" <> show i
  where
    root = "1852H/1815H/0H/"
