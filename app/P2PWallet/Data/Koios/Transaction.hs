{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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
import P2PWallet.Data.Plutus

-- | The type respesenting the UTxOs returned as part of the tx_info API endpoint.
data TransactionUTxO = TransactionUTxO
  { _paymentAddress :: PaymentAddress
  , _stakeAddress :: Maybe StakeAddress
  , _utxoRef :: TxOutRef
  , _lovelaces :: Lovelace
  , _datumHash :: Maybe Text
  , _inlineDatum :: Maybe Value
  , _referenceScriptHash :: Maybe Text
  , _nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

instance Pretty TransactionUTxO where
  pretty TransactionUTxO{..} = align $
    vsep [ "Payment Address:" <+> pretty _paymentAddress
         , "Stake Address:" <+> maybe "none" pretty _stakeAddress
         , "Output Reference:" <+> pretty @Text (showTxOutRef _utxoRef)
         , "Value:" <+> pretty @String (printf "%D ADA" $ toADA _lovelaces)
         , "Datum Hash:" <+> maybe "none" pretty _datumHash 
         , "Inline Datum:" <+> maybe "none" (pretty . showValue) _inlineDatum
         , "Reference Script Hash:" <+> maybe "none" pretty _referenceScriptHash
         , if null _nativeAssets 
           then "Native Assets: none"
           else vsep [ "Native Assets:"
                     , indent 4 $ align $ vsep $ map pretty _nativeAssets
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
      { _poolId :: PoolID
      , _stakeAddress :: StakeAddress
      }
  -- Both Registration and Deregistration have the same info.
  | StakeRegistrationInfo
      { _stakeAddress :: StakeAddress
      }
  | OtherInfo Value
  deriving (Show,Eq)

instance Pretty CertificateInfo where
  pretty (OtherInfo v) = "Info:" <+> pretty (showValue v)
  pretty StakeRegistrationInfo{..} = "Stake Address:" <+> pretty (toText _stakeAddress)
  pretty DelegationInfo{..} = 
    vsep [ "Pool ID:" <+> pretty (toText _poolId)
         , "Stake Address:" <+> pretty (toText _stakeAddress)
         ]

instance FromJSON CertificateInfo where
  parseJSON value = pure $ fromMaybe (OtherInfo value) 
                  $ parseMaybe delegationInfoParser value 
                <|> parseMaybe stakeRegistrationInfoParser value
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
  { _type :: CertificateType
  , _info :: CertificateInfo
  } deriving (Show,Eq)

instance FromJSON TransactionCertificate where
  parseJSON =
    withObject "TransactionCertificate" $ \o ->
      TransactionCertificate
        <$> (o .: "type" >>= maybe mzero return . readCertificateType)
        <*> o .: "info"

instance Pretty TransactionCertificate where
  pretty TransactionCertificate{..} =
    vsep [ "Type:" <+> pretty _type
         , pretty _info
         ]

-- | The type respesenting the overall information returned with the tx_info query.
data Transaction = Transaction
  { _txHash :: Text
  , _blockTime :: POSIXTime
  , _blockHeight :: Integer
  , _fee :: Lovelace
  , _size :: Integer
  , _deposit :: Lovelace
  , _invalidBefore :: Maybe Text
  , _invalidAfter :: Maybe Text
  , _collateralInputs :: [TransactionUTxO]
  , _collateralOutput :: Maybe TransactionUTxO
  , _referenceInputs :: [TransactionUTxO]
  , _inputs :: [TransactionUTxO]
  , _outputs :: [TransactionUTxO]
  , _certificates :: [TransactionCertificate]
  -- , _withdrawals :: Value
  -- , _nativeAssetsMinted :: Value
  -- , _nativeScripts :: Value
  -- , _plutusContracts :: Value
  } deriving (Show,Eq)

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
        -- <*> o .: "withdrawals"
        -- <*> o .: "assets_minted"
        -- <*> o .: "native_scripts"
        -- <*> o .: "plutus_contracts"
