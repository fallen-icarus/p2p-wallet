{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Witness where

import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Plutus
import P2PWallet.Prelude

newtype Witness = Witness { witness :: (PubKeyHash, Maybe DerivationPath) }
  deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''Witness
