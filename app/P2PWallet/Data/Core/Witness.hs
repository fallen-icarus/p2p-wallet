{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

{-
 
Stake keys being registered must be handled differently than stake keys for any other
action. The reason for this is that cardano-hw-cli requires the stake keys being registered
to be part of every witness action, even though they do not generate their own witness file
output. All other actions require a separate witness file output for each key. You can
read more about it [here](https://github.com/vacuumlabs/cardano-hw-cli/issues/163).

-}
module P2PWallet.Data.Core.Witness where

import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Data.Plutus
import P2PWallet.Prelude

-- | Witnesses that must be part of every `cardano-hw-cli transaction witness` command even
-- though they do not produce their own witness output file.
newtype RegistrationWitness = RegistrationWitness
  { _witness :: (PubKeyHash, Maybe DerivationPath) }
  deriving (Show,Eq,Ord)

-- | Witnesses that do not need to be part of every `cardano-hw-cli transaction witness` command
-- and do produce their own witness output file.
newtype NormalWitness = NormalWitness
  { _witness :: (PubKeyHash, Maybe DerivationPath) }
  deriving (Show,Eq,Ord)
