{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/tx_info API endpoint 
(the types are the same for mainnet). The UTxOs returned with this endpoint are uniquely structured
which is why there is a dedicated `TransactionUTxO` type.

-}
module P2PWallet.Data.Koios.Transaction where

import Data.Aeson
import Data.Aeson.Types
import Prettyprinter

import P2PWallet.Prelude
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.PoolID
import P2PWallet.Plutus

-- | The type respesenting the UTxOs returned as part of the tx_info API endpoint.
data TransactionUTxO = TransactionUTxO
  { paymentAddress :: PaymentAddress
  , stakeAddress :: Maybe StakeAddress
  , utxoRef :: TxOutRef
  , lovelaces :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionUTxO

instance Pretty TransactionUTxO where
  pretty TransactionUTxO{..} = align $
    vsep [ "Payment Address:" <+> pretty paymentAddress
         , "Stake Address:" <+> maybe "none" pretty stakeAddress
         , "Output Reference:" <+> pretty @Text (showTxOutRef utxoRef)
         , "Value:" <+> pretty @String (printf "%D ADA" $ toAda lovelaces)
         , "Datum Hash:" <+> maybe "none" pretty datumHash 
         , "Inline Datum:" <+> maybe "none" (pretty . showValue) inlineDatum
         , "Reference Script Hash:" <+> maybe "none" pretty referenceScriptHash
         , if null nativeAssets 
           then "Native Assets: none"
           else vsep [ "Native Assets:"
                     , indent 4 $ align $ vsep $ map pretty nativeAssets
                     ]
         ]

instance FromJSON TransactionUTxO where
  parseJSON =
      withObject "TransactionUTxO" $ \o ->
        TransactionUTxO
          <$> (o .: "payment_addr" >>= withObject "payment_addr" (\o' -> o' .: "bech32"))
          <*> o .: "stake_addr"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . readTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "inline_datum" >>= 
                maybe (return Nothing) (withObject "inline_datum" $ \i -> i .: "value"))
          <*> (o .: "reference_script" >>= 
                maybe (return Nothing) (withObject "reference_script" $ \i -> i .: "hash"))
          <*> o .: "asset_list"
    where
      concatRef :: Text -> Integer -> Text
      concatRef hash idx = hash <> "#" <> show idx

data CertificateType
  = DelegationType
  | StakeRegistrationType
  | StakeDeregistrationType
  | PoolUpdateType
  | PoolRetireType
  | ParamProposalType
  | ReserveMirType
  | TreasuryMirType
  deriving (Show,Eq)

makePrisms ''CertificateType

instance Pretty CertificateType where
  pretty DelegationType = "Delegation"
  pretty StakeRegistrationType = "Stake Registration"
  pretty StakeDeregistrationType = "Stake Deregistration"
  pretty PoolUpdateType = "Pool Update"
  pretty PoolRetireType = "Pool Retire"
  pretty ReserveMirType = "Reserve MIR"
  pretty TreasuryMirType = "Treasury MIR"
  pretty ParamProposalType = "Param Proposal"

readCertificateType :: Text -> Maybe CertificateType
readCertificateType "delegation" = Just DelegationType
readCertificateType "stake_registration" = Just StakeRegistrationType
readCertificateType "stake_deregistration" = Just StakeDeregistrationType
readCertificateType "pool_update" = Just PoolUpdateType
readCertificateType "pool_retire" = Just PoolRetireType
readCertificateType "reserve_MIR" = Just ReserveMirType
readCertificateType "treasury_MIR" = Just TreasuryMirType
readCertificateType "param_proposal" = Just ParamProposalType
readCertificateType _ = Nothing

data CertificateInfo
  = DelegationInfo
      { poolId :: PoolID
      , stakeAddress :: StakeAddress
      }
  -- Both Registration and Deregistration have the same info.
  | StakeRegistrationInfo
      { stakeAddress :: StakeAddress
      }
  | OtherInfo Value
  deriving (Show,Eq)

makePrisms ''CertificateInfo

instance Pretty CertificateInfo where
  pretty (OtherInfo v) = "Info:" <+> pretty (showValue v)
  pretty StakeRegistrationInfo{..} = "Stake Address:" <+> pretty (toText stakeAddress)
  pretty DelegationInfo{..} = 
    vsep [ "Pool ID:" <+> pretty (toText poolId)
         , "Stake Address:" <+> pretty (toText stakeAddress)
         ]

instance FromJSON CertificateInfo where
  parseJSON value = 
      pure $ fromMaybe (OtherInfo value) $ asum
        [ parseMaybe delegationInfoParser value 
        , parseMaybe stakeRegistrationInfoParser value
        ]
    where
      delegationInfoParser :: Value -> Parser CertificateInfo
      delegationInfoParser = withObject "DelegationInfo" $ \o ->
        DelegationInfo
          <$> o .: "pool_id_bech32"
          <*> o .: "stake_address"

      stakeRegistrationInfoParser :: Value -> Parser CertificateInfo
      stakeRegistrationInfoParser = withObject "StakeRegistrationInfo" $ \o ->
        StakeRegistrationInfo
          <$> o .: "stake_address"

data TransactionCertificate = TransactionCertificate
  { certType :: CertificateType
  , info :: CertificateInfo
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionCertificate

instance FromJSON TransactionCertificate where
  parseJSON =
    withObject "TransactionCertificate" $ \o ->
      TransactionCertificate
        <$> (o .: "type" >>= maybe mzero return . readCertificateType)
        <*> o .: "info"

instance Pretty TransactionCertificate where
  pretty TransactionCertificate{..} =
    vsep [ "Type:" <+> pretty certType
         , pretty info
         ]

-- | The type representing withdrawal information in a transaction.
data TransactionWithdrawal = TransactionWithdrawal
  { lovelaces :: Lovelace
  , stakeAddress :: StakeAddress
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionWithdrawal

instance FromJSON TransactionWithdrawal where
  parseJSON = withObject "TransactionWithdrawal" $ \o ->
    TransactionWithdrawal
      <$> o .: "amount"
      <*> o .: "stake_addr"

instance Pretty TransactionWithdrawal where
  pretty TransactionWithdrawal{..} =
    vsep [ "Stake Address:" <+> pretty stakeAddress
         , "Value:" <+> fromString (printf "%D ADA" $ toAda lovelaces)
         ]

-- | The type respesenting the overall information returned with the tx_info query.
data Transaction = Transaction
  { txHash :: Text
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  , fee :: Lovelace
  , size :: Integer
  , deposit :: Lovelace
  , invalidBefore :: Maybe Text
  , invalidAfter :: Maybe Text
  , collateralInputs :: [TransactionUTxO]
  , collateralOutput :: Maybe TransactionUTxO
  , referenceInputs :: [TransactionUTxO]
  , inputs :: [TransactionUTxO]
  , outputs :: [TransactionUTxO]
  , certificates :: [TransactionCertificate]
  , withdrawals :: [TransactionWithdrawal]
  -- , nativeAssetsMinted :: Value
  -- , nativeScripts :: Value
  -- , plutusContracts :: Value
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Transaction

instance FromJSON Transaction where
  parseJSON =
    withObject "Transaction" $ \o ->
      Transaction
        <$> o .: "tx_hash"
        <*> o .: "tx_timestamp"
        <*> o .: "block_height"
        <*> o .: "fee"
        <*> o .: "tx_size"
        <*> o .: "deposit"
        <*> o .: "invalid_before"
        <*> o .: "invalid_after"
        <*> o .: "collateral_inputs"
        <*> o .: "collateral_output"
        <*> o .: "reference_inputs"
        <*> o .: "inputs"
        <*> o .: "outputs"
        <*> o .: "certificates"
        <*> o .: "withdrawals"
        -- <*> o .: "assets_minted"
        -- <*> o .: "native_scripts"
        -- <*> o .: "plutus_contracts"
