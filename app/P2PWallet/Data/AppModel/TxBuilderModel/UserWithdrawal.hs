{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.UserWithdrawal where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- User Withdrawals
-------------------------------------------------
-- | Information for a user withdrawal.
data UserWithdrawal = UserWithdrawal
  -- | The bech32 address for the associated stake credential. This is used to get any required 
  -- key hashes.
  { stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount withdrawn.
  , lovelace :: Lovelace
  -- | The alias for this stake address.
  , walletAlias :: Text
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''UserWithdrawal

instance AddToTxBody UserWithdrawal where
  addToTxBody txBody UserWithdrawal{stakeAddress,lovelace,stakeKeyDerivation} =
      txBody 
        -- Add the new withdrawal. They will be reordered by the node.
        & #withdrawals %~ (newWithdrawal:)
        -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
        -- instance of `TxBody`. They will also be sorted.
        & #keyWitnesses %~ maybe id (:) requiredWitness
    where 
      newWithdrawal :: TxBodyWithdrawal
      newWithdrawal = TxBodyWithdrawal
        { stakeAddress = stakeAddress
        , lovelace = lovelace
        , stakeCredential = stakeCredential
        , stakingScriptInfo = Nothing
        }

      stakeCredential :: Credential
      stakeCredential =
        -- This should have been verified at an earlier step.
        fromRight (PubKeyCredential "") $ stakeAddressToPlutusCredential stakeAddress

      requiredWitness :: Maybe KeyWitness
      requiredWitness = case stakeCredential of
          ScriptCredential _ -> Nothing
          PubKeyCredential pkHash -> Just $ KeyWitness (pkHash, stakeKeyDerivation)
