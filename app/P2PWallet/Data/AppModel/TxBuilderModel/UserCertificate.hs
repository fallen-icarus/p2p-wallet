{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.UserCertificate where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- User Certificates
-------------------------------------------------
-- | Information for a user certificate.
data UserCertificate = UserCertificate
  -- | The bech32 address for the associated stake credential. This is used to get any required 
  -- key hashes.
  { stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , stakeKeyPath :: Maybe DerivationPath 
  -- | The certificate action.
  , certificateAction :: CertificateAction
  -- | The alias for this stake address.
  , walletAlias :: Text
  -- | The name of the pool about to be delegated to.
  , poolName :: Maybe Text
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''UserCertificate

instance AddToTxBody UserCertificate where
  addToTxBody txBody UserCertificate{stakeAddress,certificateAction,stakeKeyPath} = 
      txBody 
        -- Add the new certificate. They will be sorted by the `Semigroup` instance of `TxBody`.
        & #certificates %~ (newCertificate:)
        -- Add the witness is required. Duplicate witnesses are removed by the `Semigroup` 
        -- instance of `TxBody`. They will also be sorted.
        & #witnesses %~ maybe id (:) requiredWitness
    where 
      newCertificate :: TxBodyCertificate
      newCertificate = TxBodyCertificate
        { stakeAddress = stakeAddress
        , certificateAction = certificateAction
        }

      stakeCredential :: Credential
      stakeCredential =
        -- This should have been verified at an earlier step.
        fromRight (PubKeyCredential "") $ stakeAddressToPlutusCredential stakeAddress

      requiredWitness :: Maybe Witness
      requiredWitness = case stakeCredential of
          ScriptCredential _ -> Nothing
          PubKeyCredential pkHash -> Just $ Witness (pkHash, stakeKeyPath)
