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
-- Certificates
-------------------------------------------------
-- | Types of certificate. When registering and delegating in the same transaction,
-- the registration certificate MUST be first.
data CertificateAction
  -- | Register a staking credential and pay the 2 ADA deposit.
  = Registration
  -- | Deregister a staking credential and recover the 2 ADA deposit.
  | Deregistration
  -- | Delegate the staking credential.
  | Delegation PoolID
  deriving (Show,Eq,Ord)

makePrisms ''CertificateAction

instance Display CertificateAction where
  display Registration = "Stake Registration"
  display Deregistration = "Stake Deregistration"
  display (Delegation (PoolID poolId)) = "Delegation to " <> toText poolId

-- | The type representing user supplied information for a certificate.
data TxBodyCertificate = TxBodyCertificate
  -- | The bech32 stake address. 
  { stakeAddress :: StakeAddress
  -- | What kind of certificate this is.
  , certificateAction :: CertificateAction
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''TxBodyCertificate

-------------------------------------------------
-- Tx Body
-------------------------------------------------
data TxBody = TxBody
  { inputs :: [TxBodyInput]
  , outputs :: [TxBodyOutput]
  , certificates :: [TxBodyCertificate]
  , witnesses :: [Witness]
  , fee :: Lovelace
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBody

instance Semigroup TxBody where
  txBody1 <> txBody2 = TxBody
    { inputs = 
        -- Add the new inputs while preserving ordering. For now, the node will reorder the inputs
        -- but this may change in the future.
        txBody1 ^. #inputs <> txBody2 ^. #inputs
    , outputs = 
        -- Add the new outputs while preserving ordering.
        txBody1 ^. #outputs <> txBody2 ^. #outputs
    , certificates = 
        -- The registration certificate must appear before the delegation certificate for a given
        -- stake address.
        sort $ txBody1 ^. #certificates <> txBody2 ^. #certificates
    , witnesses = 
        -- Remove duplicates.
        ordNub $ txBody1 ^. #witnesses <> txBody2 ^. #witnesses
    , fee = 
        -- Most txBodies will have a fee of zero so the non-zero one must be chosen.
        -- I do not think there is a case where two txBodies will both have fees set.
        max (txBody1 ^. #fee) (txBody2 ^. #fee)
    }

instance Monoid TxBody where
  mempty = TxBody
    { inputs = []
    , outputs = []
    , certificates = []
    , witnesses = []
    , fee = 0
    }

class AddToTxBody a where
  -- Some highlevel actions impact multiple parts of the transaction.
  addToTxBody :: TxBody -> a -> TxBody
