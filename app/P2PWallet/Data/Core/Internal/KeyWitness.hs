{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.KeyWitness where

import P2PWallet.Data.Core.Internal.KeyDerivation
import P2PWallet.Plutus
import P2PWallet.Prelude

-- The `Text` is the user provided alias for the credential. Payment keys use the alias for the
-- associated `PaymentWallet`.
newtype KeyWitness = KeyWitness { unKeyWitness :: (Text, PubKeyHash, Maybe DerivationInfo) }
  deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''KeyWitness
