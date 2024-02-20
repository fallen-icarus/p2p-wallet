{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module P2PWallet.Data.Lens where

import Control.Lens (lens,makeFieldsNoPrefix,makePrisms)

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Koios.StakeAccount
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Data.App
-------------------------------------------------
makeFieldsNoPrefix ''AppModel
makeFieldsNoPrefix ''AssetFilters
makeFieldsNoPrefix ''Config
makeFieldsNoPrefix ''DelegationModel
makeFieldsNoPrefix ''ExpandedFields
makeFieldsNoPrefix ''HomeModel
makeFieldsNoPrefix ''NewPaymentWallet
makeFieldsNoPrefix ''NewStakeWallet
makeFieldsNoPrefix ''TransactionFilters
makeFieldsNoPrefix ''TxBuilderModel
makeFieldsNoPrefix ''UserCertificate
makeFieldsNoPrefix ''UserChangeOutput
makeFieldsNoPrefix ''UserFilters
makeFieldsNoPrefix ''UserInput
makeFieldsNoPrefix ''UserOutput
makeFieldsNoPrefix ''UserPoolFilters
makeFieldsNoPrefix ''UserWithdrawal
makeFieldsNoPrefix ''UTxOFilters
makeFieldsNoPrefix ''VerifiedCertificate
makeFieldsNoPrefix ''VerifiedChangeOutput
makeFieldsNoPrefix ''VerifiedFilters
makeFieldsNoPrefix ''VerifiedInput
makeFieldsNoPrefix ''VerifiedOutput
makeFieldsNoPrefix ''VerifiedWithdrawal

-------------------------------------------------
-- Data.Core
-------------------------------------------------
makeFieldsNoPrefix ''NativeAsset
makeFieldsNoPrefix ''NormalWitness
makeFieldsNoPrefix ''RegistrationWitness

makePrisms ''DerivationPath

-------------------------------------------------
-- Data.Koios
-------------------------------------------------
makeFieldsNoPrefix ''AddressUTxO
makeFieldsNoPrefix ''Pool
makeFieldsNoPrefix ''PoolInfo
makeFieldsNoPrefix ''StakeAccount
makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''TransactionUTxO

-------------------------------------------------
-- Data.Wallets
-------------------------------------------------
makeFieldsNoPrefix ''PaymentWallet
makeFieldsNoPrefix ''StakeWallet
makeFieldsNoPrefix ''Wallets

-------------------------------------------------
-- Miscellaneous Functions
-------------------------------------------------
-- | A lens that interprets a `Maybe a` as True or False. This is usefull for widgets
-- that depend on a `Bool`.
boolLens :: a -> ALens' s (Maybe a) -> ALens' s Bool
boolLens def' targetLens = 
  lens (\m -> isJust $ m ^# targetLens)
       (\m b -> m & targetLens #~ if b then Just def' else Nothing)

maybeLens :: a -> ALens' s (Maybe a) -> ALens' s a
maybeLens def' targetLens = 
  lens (\m -> fromMaybe def' $ m ^# targetLens)
       (\m t -> m & targetLens #~ Just t)

-- | A lens into a sum type `a` with a default choice of type `b`. The unWrapper `(a -> b)`
-- must cover the cases where the other data constructors are present. This is useful for
-- editing the inner `Text` for types like `CertificateAction`.
sumsOfProductsLens :: (a -> b) -> (b -> a) -> ALens' s a -> ALens' s b
sumsOfProductsLens unWrapper wrapper targetLens =
  lens (\m -> unWrapper $ m ^# targetLens)
       (\m t -> m & targetLens #~ wrapper t)

hasAssetWithFingerprint :: (HasNativeAssets a [NativeAsset]) => Text -> a -> Bool
hasAssetWithFingerprint finger u =
  isJust $ find (\a -> a ^. fingerprint == finger) $ u ^. nativeAssets

hasAssetWithTokenName :: (HasNativeAssets a [NativeAsset]) => Text -> a -> Bool
hasAssetWithTokenName name' u =
  isJust $ find (\a -> a ^. tokenName == name') $ u ^. nativeAssets

hasAssetWithPolicyId :: (HasNativeAssets a [NativeAsset]) => Text -> a -> Bool
hasAssetWithPolicyId policy u =
  isJust $ find (\a -> a ^. policyId == policy) $ u ^. nativeAssets

fullAssetName :: NativeAsset -> Text
fullAssetName a = a ^. policyId <> "." <> a ^. tokenName

rawAdas :: HasLovelaces s Integer => Lens' s Decimal
rawAdas = 
  lens (\m -> unADA $ toADA $ Lovelace $ m ^. lovelaces)
       (\m n -> m & lovelaces .~ unLovelace (toLovelace $ ADA n))

isDelegation :: CertificateAction -> Bool
isDelegation (Delegation _) = True
isDelegation _ = False
