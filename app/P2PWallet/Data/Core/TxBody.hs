{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The intermediate representation for a transaction. The transaction builder model will be converted
to this representation before building. It is effectively a medium-level representation for a
transaction. It is assumed that the inputs and outputs are already balanced before converting to
`TxBody`.

-}
module P2PWallet.Data.Core.TxBody where

import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Inputs
-------------------------------------------------
-- | Information for a particular input.
newtype TxBodyInput = TxBodyInput
  -- | The input's output reference.
  { utxoRef :: TxOutRef
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBodyInput

-------------------------------------------------
-- Outputs
-------------------------------------------------
-- | Information for a particular output.
data TxBodyOutput = TxBodyOutput
  -- | The target address.
  { paymentAddress :: PaymentAddress
  -- | The amount of lovelace to deposit for this output.
  , lovelace :: Lovelace
  -- | The native assets to store in this output.
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBodyOutput

-------------------------------------------------
-- Tx Body
-------------------------------------------------
data TxBody = TxBody
  { inputs :: [TxBodyInput]
  , outputs :: [TxBodyOutput]
  , witnesses :: [Witness]
  , fee :: Lovelace
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBody

instance Semigroup TxBody where
  txBody1 <> txBody2 = TxBody
    { inputs = txBody1 ^. #inputs <> txBody2 ^. #inputs
    , outputs = txBody1 ^. #outputs <> txBody2 ^. #outputs
    , witnesses = ordNub $ txBody1 ^. #witnesses <> txBody2 ^. #witnesses -- remove duplicates
    , fee = max (txBody1 ^. #fee) (txBody2 ^. #fee)
    }

instance Monoid TxBody where
  mempty = TxBody
    { inputs = []
    , outputs = []
    , witnesses = []
    , fee = 0
    }

class AddToTxBody a where
  -- Some highlevel actions impact multiple parts of the transaction.
  addToTxBody :: TxBody -> a -> TxBody

class RequiredWitness a where
  -- Not all actions _always_ require witnesses.
  requiredWitness :: a -> Maybe Witness
