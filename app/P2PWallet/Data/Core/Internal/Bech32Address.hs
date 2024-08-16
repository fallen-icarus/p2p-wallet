{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.Bech32Address
  ( -- * Bech32 Encoded Addresses
    PaymentAddress(..)
  , parsePaymentAddress
  , StakeAddress(..)
  , parseStakeAddress

    -- * Conversions
  , plutusToBech32
  , paymentAddressToPlutusAddress
  , stakeAddressToPlutusCredential
  , paymentAddressStakeCredential

    -- * Inspecting Bech32 Addresses
  , Address.AddressInfo(..)
  , inspectBech32Address

    -- * PubKey Check
  , isPubKeyOnlyAddress
  ) where

import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Maybe (fromJust)

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import Cardano.Address.Style.Shelley qualified as Address
import Cardano.Address (fromBech32,bech32,bech32With)
import Cardano.Address.Script qualified as Address
import Cardano.Address.Derivation (Depth(..))
import Cardano.Codec.Bech32.Prefixes (stake_test,stake)

import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Plutus as PV1
import P2PWallet.Prelude

-------------------------------------------------
-- Bech32 Encoded Payment Addresses
-------------------------------------------------
-- | A wrapper around a bech32 encoded payment address.
newtype PaymentAddress = PaymentAddress { unPaymentAddress :: Text }
  deriving (Show)
  deriving newtype (Eq,Ord,ToField,FromField,IsString,ToJSON,FromJSON,ToText,ToString)

instance Display PaymentAddress where
  display = toText

makeFieldLabelsNoPrefix ''PaymentAddress

-------------------------------------------------
-- Bech32 Encoded Stake Addresses
-------------------------------------------------
-- | A wrapper around a bech32 encoded stake address.
newtype StakeAddress = StakeAddress { unStakeAddress :: Text }
  deriving (Show)
  deriving newtype (Eq,Ord,ToField,FromField,IsString,ToJSON,FromJSON,ToText,ToString)

instance Display StakeAddress where
  display = toText

makeFieldLabelsNoPrefix ''StakeAddress

-------------------------------------------------
-- Inspecting Bech32 Addresses
-------------------------------------------------
-- | Try to look inside a bech32 address to determine the credential hashes.
inspectBech32Address :: Text -> Either Text Address.AddressInfo
inspectBech32Address addr = do
    bechAddr <- maybeToRight "Not a valid bech32 address." $ fromBech32 addr

    inspectInfo <- 
      first (const "Could not inspect bech32 address.") $
        Address.eitherInspectAddress Nothing bechAddr

    case inspectInfo of
      Address.InspectAddressShelley info -> Right info
      _ -> Left "Bech32 address is not a shelley address."

-------------------------------------------------
-- Parsing Bech32 Addresses
-------------------------------------------------
-- | Try to convert a user supplied payment address to `PaymentAddress`. Since bech32 addresses
-- also encode the network used, this function requires specifying what network to expect
-- with the address. Only shelley addresses are supported.
parsePaymentAddress :: Network -> Text -> Either Text PaymentAddress
parsePaymentAddress targetNetwork rawAddr = do
    Address.AddressInfo{infoSpendingKeyHash,infoScriptHash,infoNetworkTag} <- 
      inspectBech32Address rawAddr

    -- The address should have exactly one payment credential. This check may not be necessary...
    when (length (catMaybes [infoScriptHash,infoSpendingKeyHash]) /= 1) $
      Left "Address is not a valid bech32 payment address."

    -- The address must be for the proper network.
    if networkTag == infoNetworkTag
    then Right $ PaymentAddress rawAddr
    else Left $ "Address is not a " <> toText targetNetwork <> " address."
  where
    networkTag = case targetNetwork of
      Mainnet -> Address.shelleyMainnet
      Testnet -> Address.shelleyTestnet

{-# WARNING parseStakeAddress "`parseStakeAddress` workaround remains in code" #-}
-- | Try to convert a user supplied stake address to `StakeAddress`. Since bech32 addresses
-- also encode the network used, this function requires specifying what network to expect
-- with the address. Only shelley addresses are supported.
parseStakeAddress :: Network -> Text -> Either Text StakeAddress
parseStakeAddress targetNetwork rawAddr = do
    Address.AddressInfo{..} <- inspectBech32Address rawAddr
    
    trace "Still need to remove workaround" $ pure ()
    -- -- The address should not have any payment credentials.
    -- when (isJust $ infoScriptHash <|> infoSpendingKeyHash) $ 
    --   Left "Address is not a stake address."
    --
    -- -- The address should have exactly one staking credential. This check may not be necessary...
    -- when (length (catMaybes [infoStakeScriptHash,infoStakeKeyHash]) /= 1) $
    --   Left "Address is not a valid bech32 stake address."

    -- WORKAROUND - the stake script hash is in `infoScriptHash` instead of `infoStakeScriptHash`.
    when (length (catMaybes [infoScriptHash,infoStakeKeyHash]) /= 1) $
      Left "Address is not a valid bech32 stake address."

    -- The address must be for the proper network.
    if networkTag == infoNetworkTag
    then Right $ StakeAddress rawAddr
    else Left $ "Address is not a " <> toText targetNetwork <> " address."
  where
    networkTag = case targetNetwork of
      Mainnet -> Address.shelleyMainnet
      Testnet -> Address.shelleyTestnet

-------------------------------------------------
-- PlutusAddress <-> Bech32
-------------------------------------------------
-- | Convert a plutus address to the corresponding bech32 addresses for the specific network. 
-- Pointer addresses are not supported.
plutusToBech32 :: Network -> PlutusAddress -> Either Text (PaymentAddress, Maybe StakeAddress)
plutusToBech32 network (PV1.Address pCred msCred) = case msCred of
    -- Create both the payment address and the stake address.
    Just sCred -> do
      stakeCred <- toBech32StakeCredential sCred
      payCred <- toBech32PaymentCredential pCred
      stakeAddr <- 
        bimap (const stakeAddrErrorMsg) (StakeAddress . bech32With stakePrefix) $
          Address.stakeAddress networkTag stakeCred 
      return ( PaymentAddress $ bech32 $ Address.delegationAddress networkTag payCred stakeCred
             , Just stakeAddr
             )
    Nothing -> do
      payCred <- toBech32PaymentCredential pCred
      return ( PaymentAddress $ bech32 $ Address.paymentAddress networkTag payCred
             , Nothing
             )
  where
    stakeAddrErrorMsg :: Text
    stakeAddrErrorMsg = "Could not create stake address from plutus StakeCredential"

    (stakePrefix, networkTag) = 
      case network of
        Mainnet -> (stake, Address.shelleyMainnet)
        Testnet -> (stake_test, Address.shelleyTestnet)
      
    toBech32StakeCredential 
      :: PV1.StakingCredential 
      -> Either Text (Address.Credential 'DelegationK)
    toBech32StakeCredential sCred = case sCred of
      PV1.StakingHash (PV1.PubKeyCredential pkh) -> 
        fmap Address.DelegationFromKeyHash $ 
          maybeToRight "Could not get stake keyHash from bytes" $ 
            Address.keyHashFromBytes 
              (Address.Delegation, unBuiltinByteString $ PV1.getPubKeyHash pkh)
      PV1.StakingHash (PV1.ScriptCredential vh) -> 
        fmap Address.DelegationFromScript $ 
          maybeToRight "Could not get stake scriptHash from bytes" $ 
            Address.scriptHashFromBytes $ unBuiltinByteString $ PV1.getScriptHash vh
      _ -> Left "Pointer addresses are not supported."

    toBech32PaymentCredential :: PV1.Credential -> Either Text (Address.Credential 'PaymentK)
    toBech32PaymentCredential payCred = case payCred of
      PV1.PubKeyCredential pkh ->
        fmap Address.PaymentFromKeyHash $ 
          maybeToRight "Could not get payment keyHash from bytes" $ 
            Address.keyHashFromBytes (Address.Payment, unBuiltinByteString $ PV1.getPubKeyHash pkh)
      PV1.ScriptCredential vh ->
        fmap Address.PaymentFromScript $ 
          maybeToRight "Could not get payment scriptHash from bytes" $ 
            Address.scriptHashFromBytes $ unBuiltinByteString $ PV1.getScriptHash vh

-- | Convert a bech32 payment address to a plutus address. This cannot be used with stake addresses
-- since plutus addresses __must__ have payment credentials.
paymentAddressToPlutusAddress :: PaymentAddress -> Either Text PlutusAddress
paymentAddressToPlutusAddress (PaymentAddress addr) = do
    Address.AddressInfo{..} <- inspectBech32Address addr
    return $ 
      PV1.Address
          { PV1.addressCredential =
              let mScriptCred = PV1.ScriptCredential 
                              . PV1.ScriptHash 
                              . BuiltinByteString 
                            <$> infoScriptHash
                  mPayCred = PV1.PubKeyCredential 
                           . PV1.PubKeyHash 
                           . BuiltinByteString
                         <$> infoSpendingKeyHash
              in fromJust $ mPayCred <|> mScriptCred
          , PV1.addressStakingCredential = case (infoStakeScriptHash,infoStakeKeyHash) of
              (Nothing, Just keyHash) -> 
                Just $ PV1.StakingHash $ PV1.PubKeyCredential $ PV1.PubKeyHash $ 
                  BuiltinByteString keyHash
              (Just scriptHash, Nothing) -> 
                Just $ PV1.StakingHash $ PV1.ScriptCredential $ PV1.ScriptHash $ 
                  BuiltinByteString scriptHash
              _ -> Nothing
          }

-- | Convert a bech32 stake address to a plutus credential (either a pubkey or a script credential). 
stakeAddressToPlutusCredential :: StakeAddress -> Either Text Credential
stakeAddressToPlutusCredential (StakeAddress addr) = do
  Address.AddressInfo{..} <- inspectBech32Address addr

  let mStakeKeyHash = 
        PV1.PubKeyCredential . PV1.PubKeyHash . BuiltinByteString <$> infoStakeKeyHash
      mStakeScriptHash = 
        PV1.ScriptCredential . PV1.ScriptHash . BuiltinByteString <$> infoStakeScriptHash

  maybeToRight "Address is not a valid bech32 stake address." $ 
    mStakeScriptHash <|> mStakeKeyHash

-- | Extract the stake credential from a bech32 payment address.
paymentAddressStakeCredential :: PaymentAddress -> Either Text Credential
paymentAddressStakeCredential (PaymentAddress addr) = do
  Address.AddressInfo{..} <- inspectBech32Address addr

  let mStakeKeyHash = 
        PV1.PubKeyCredential . PV1.PubKeyHash . BuiltinByteString <$> infoStakeKeyHash
      mStakeScriptHash = 
        PV1.ScriptCredential . PV1.ScriptHash . BuiltinByteString <$> infoStakeScriptHash

  maybeToRight "Address does not have a staking credential." $ mStakeScriptHash <|> mStakeKeyHash

-------------------------------------------------
-- PubKeyCredential Check
-------------------------------------------------
-- | The app currently only supports pubkey user credentials, even for watched wallets.
isPubKeyOnlyAddress :: Text -> Either Text ()
isPubKeyOnlyAddress addr = do
  Address.AddressInfo{..} <- inspectBech32Address addr
  let mStakeScriptHash = 
        PV1.ScriptCredential . PV1.ScriptHash . BuiltinByteString <$> infoStakeScriptHash
      mScriptHash = 
        PV1.ScriptCredential . PV1.ScriptHash . BuiltinByteString <$> infoScriptHash

  when (isJust $ mStakeScriptHash <|> mScriptHash) $ Left "Only pubkey credentials are supported."
