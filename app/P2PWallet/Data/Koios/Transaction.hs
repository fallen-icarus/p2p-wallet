{-# LANGUAGE DataKinds #-}
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
import Data.Aeson.Types (Parser, parseMaybe)

import P2PWallet.Prelude
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Internal.DRepID
import P2PWallet.Data.Core.Internal.PoolID
import P2PWallet.Plutus

-------------------------------------------------
-- TransactionUTxO
-------------------------------------------------
-- | The type respesenting the UTxOs returned as part of the tx_info API endpoint.
data TransactionUTxO = TransactionUTxO
  { paymentAddress :: PaymentAddress
  , stakeAddress :: Maybe StakeAddress
  , utxoRef :: TxOutRef
  , lovelace :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionUTxO

instance FromJSON TransactionUTxO where
  parseJSON =
      withObject "TransactionUTxO" $ \o ->
        TransactionUTxO
          <$> (o .: "payment_addr" >>= withObject "payment_addr" (.: "bech32"))
          <*> o .: "stake_addr"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . parseTxOutRef)
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

-------------------------------------------------
-- TransactionCertificate
-------------------------------------------------
data CertificateType
  = DelegationCertificate
  | StakeRegistrationCertificate
  | StakeDeregistrationCertificate
  | PoolUpdateCertificate
  | PoolRetireCertificate
  | ParamProposalCertificate
  | ReserveMirCertificate
  | TreasuryMirCertificate
  | VoteDelegationCertificate
  deriving (Show,Eq)

instance Display CertificateType where
  display DelegationCertificate = "Stake Delegation"
  display StakeRegistrationCertificate = "Stake Registration"
  display StakeDeregistrationCertificate = "Stake Deregistration"
  display PoolUpdateCertificate = "Pool Update"
  display PoolRetireCertificate = "Pool Retirement"
  display ParamProposalCertificate = "Parameter Proposal"
  display ReserveMirCertificate = "Reserve MIR"
  display TreasuryMirCertificate = "Treasury MIR"
  display VoteDelegationCertificate = "Vote Delegation"

parseCertificateType :: Text -> Maybe CertificateType
parseCertificateType "pool_delegation" = Just DelegationCertificate
parseCertificateType "stake_registration" = Just StakeRegistrationCertificate
parseCertificateType "stake_deregistration" = Just StakeDeregistrationCertificate
parseCertificateType "pool_update" = Just PoolUpdateCertificate
parseCertificateType "pool_retire" = Just PoolRetireCertificate
parseCertificateType "reserve_MIR" = Just ReserveMirCertificate
parseCertificateType "treasury_MIR" = Just TreasuryMirCertificate
parseCertificateType "param_proposal" = Just ParamProposalCertificate
parseCertificateType "vote_delegation" = Just VoteDelegationCertificate
parseCertificateType _ = Nothing

showCertificateType :: CertificateType -> Text
showCertificateType DelegationCertificate = "pool_delegation"
showCertificateType StakeRegistrationCertificate = "stake_registration"
showCertificateType StakeDeregistrationCertificate = "stake_deregistration"
showCertificateType PoolUpdateCertificate = "pool_update"
showCertificateType PoolRetireCertificate = "pool_retire"
showCertificateType ParamProposalCertificate = "param_proposal"
showCertificateType ReserveMirCertificate = "reserve_MIR"
showCertificateType TreasuryMirCertificate = "treasury_MIR"
showCertificateType VoteDelegationCertificate = "vote_delegation"

data CertificateInfo
  = StakeDelegationInfo { poolId :: PoolID , stakeAddress :: StakeAddress }
  -- Both Registration and Deregistration have the same info.
  | StakeRegistrationInfo { stakeAddress :: StakeAddress }
  | VoteDelegationInfo { drepId :: DRepID , stakeAddress :: StakeAddress }
  | OtherInfo Value
  deriving (Show,Eq)

makePrisms ''CertificateInfo

instance FromJSON CertificateInfo where
  parseJSON value = 
      pure $ fromMaybe (OtherInfo value) $ asum
        [ parseMaybe stakeDelegationInfoParser value 
        , parseMaybe voteDelegationInfoParser value 
        , parseMaybe stakeRegistrationInfoParser value
        ]
    where
      stakeDelegationInfoParser :: Value -> Parser CertificateInfo
      stakeDelegationInfoParser = withObject "StakeDelegationInfo" $ \o ->
        StakeDelegationInfo
          <$> o .: "pool_id_bech32"
          <*> o .: "stake_address"

      voteDelegationInfoParser :: Value -> Parser CertificateInfo
      voteDelegationInfoParser = withObject "VoteDelegationInfo" $ \o ->
        VoteDelegationInfo
          <$> o .: "drep_id"
          <*> o .: "stake_address"

      stakeRegistrationInfoParser :: Value -> Parser CertificateInfo
      stakeRegistrationInfoParser = withObject "StakeRegistrationInfo" $ \o ->
        StakeRegistrationInfo
          <$> o .: "stake_address"

instance ToJSON CertificateInfo where
  toJSON StakeDelegationInfo{..} =
    object [ "pool_id_bech32" .= poolId
           , "stake_address" .= stakeAddress
           ]

  toJSON VoteDelegationInfo{..} =
    object [ "drep_id" .= drepId
           , "stake_address" .= stakeAddress
           ]

  toJSON StakeRegistrationInfo{..} =
    object [ "stake_address" .= stakeAddress ]

  toJSON (OtherInfo value) = value

data TransactionCertificate = TransactionCertificate
  { certificateType :: CertificateType
  , info :: CertificateInfo
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionCertificate

instance FromJSON TransactionCertificate where
  parseJSON =
    withObject "TransactionCertificate" $ \o ->
      TransactionCertificate
        <$> (o .: "type" >>= maybe mzero return . parseCertificateType)
        <*> o .: "info"

instance ToJSON TransactionCertificate where
  toJSON TransactionCertificate{..} =
    object [ "type" .= showCertificateType certificateType
           , "info" .= info
           ]

-------------------------------------------------
-- TransactionWithdrawal
-------------------------------------------------
-- | The type representing withdrawal information in a transaction.
data TransactionWithdrawal = TransactionWithdrawal
  { lovelace :: Lovelace
  , stakeAddress :: StakeAddress
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionWithdrawal

instance FromJSON TransactionWithdrawal where
  parseJSON = withObject "TransactionWithdrawal" $ \o ->
    TransactionWithdrawal
      <$> o .: "amount"
      <*> o .: "stake_addr"

instance ToJSON TransactionWithdrawal where
  toJSON TransactionWithdrawal{..} =
    object [ "amount" .= lovelace
           , "stake_addr" .= stakeAddress
           ]

-------------------------------------------------
-- TransactionPlutusContract
-------------------------------------------------
-- | The purpose for the contract execution.
data ContractPurpose
  = SpendPurpose
  | MintPurpose
  | RewardPurpose
  | OtherPurpose Text
  deriving (Show,Eq)

instance FromJSON ContractPurpose where
  parseJSON = withText "ContractPurpose" $ \case
    "spend" -> return SpendPurpose
    "mint" -> return MintPurpose
    "reward" -> return RewardPurpose
    x -> return $ OtherPurpose x

instance ToJSON ContractPurpose where
  toJSON SpendPurpose = toJSON @Text "spend"
  toJSON MintPurpose = toJSON @Text "mint"
  toJSON RewardPurpose = toJSON @Text "reward"
  toJSON (OtherPurpose x) = toJSON x

instance Display ContractPurpose where
  display SpendPurpose = "Spending"
  display MintPurpose = "Minting/Burning"
  display RewardPurpose = "Rewards Withdrawal"
  display (OtherPurpose x) = "Other Purpose: " <> x

-- | The type representing plutus contract execution information.
data TransactionPlutusContract = TransactionPlutusContract
  { paymentAddress :: Maybe PaymentAddress
  , spendsInput :: Maybe TxOutRef
  , scriptHash :: Text
  , datum :: Maybe Value
  , redeemer :: Value
  , purpose :: ContractPurpose
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TransactionPlutusContract

instance FromJSON TransactionPlutusContract where
  parseJSON = withObject "TransactionPlutusContract" $ \o ->
      TransactionPlutusContract
        <$> o .: "address"
        <*> (o .:? "spends_input" >>= parseInputRef)
        <*> o .: "script_hash"
        <*> (o .: "input" >>= (.: "datum") >>= maybe (return Nothing) (.: "value"))
        <*> (o .: "input" >>= (.: "redeemer") >>= (.: "datum") >>= (.: "value"))
        <*> (o .: "input" >>= (.: "redeemer") >>= (.: "purpose"))
    where
      concatRef :: Text -> Integer -> Text
      concatRef hash idx = hash <> "#" <> show idx

      parseInputRef :: Maybe Value -> Parser (Maybe TxOutRef)
      parseInputRef Nothing = return Nothing
      parseInputRef (Just val) = flip (withObject "spends_input") val $ \o -> do
        res <- concatRef <$> o .: "tx_hash" <*> o .: "tx_index"
        maybe mzero (return . Just) $ parseTxOutRef res

-------------------------------------------------
-- Transaction
-------------------------------------------------
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
  , referenceInputs :: [TransactionUTxO]
  , inputs :: [TransactionUTxO]
  , outputs :: [TransactionUTxO]
  , certificates :: [TransactionCertificate]
  , withdrawals :: [TransactionWithdrawal]
  , mints :: [NativeAsset]
  , plutusContracts :: [TransactionPlutusContract]
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
        <*> o .: "reference_inputs"
        <*> o .: "inputs"
        <*> o .: "outputs"
        <*> o .: "certificates"
        <*> o .: "withdrawals"
        <*> o .: "assets_minted"
        <*> o .: "plutus_contracts"

-------------------------------------------------
-- Event Transaction
-------------------------------------------------
-- | The type respesenting only some of the information returned with the tx_info query. This
-- is useful for determining a chain of events for the protocols.
data EventTransaction = EventTransaction
  { txHash :: Text
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  , inputs :: [TransactionUTxO]
  , outputs :: [TransactionUTxO]
  , mints :: [NativeAsset]
  , plutusContracts :: [TransactionPlutusContract]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''EventTransaction

instance FromJSON EventTransaction where
  parseJSON =
    withObject "EventTransaction" $ \o ->
      EventTransaction
        <$> o .: "tx_hash"
        <*> o .: "tx_timestamp"
        <*> o .: "block_height"
        <*> o .: "inputs"
        <*> o .: "outputs"
        <*> o .: "assets_minted"
        <*> o .: "plutus_contracts"
