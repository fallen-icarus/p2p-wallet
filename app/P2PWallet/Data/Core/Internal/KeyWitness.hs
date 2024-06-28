{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.KeyWitness where

import P2PWallet.Data.Core.Internal.DerivationPath
import P2PWallet.Plutus
import P2PWallet.Prelude

newtype KeyWitness = KeyWitness { unKeyWitness :: (PubKeyHash, Maybe DerivationPath) }
  deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''KeyWitness
