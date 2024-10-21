{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Proposal Close
-------------------------------------------------
-- | Information for closing an proposal.
data ProposalClose = ProposalClose
  { utxoRef :: TxOutRef
  -- | The stake credential for this writer.
  , writerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current proposal terms.
  , proposalDatum :: Maybe Options.ProposalDatum
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Which network this is for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ProposalClose

instance AssetBalancesForChange (a,ProposalClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= proposalBeaconCurrencySymbol

optionsUTxOToProposalClose 
  :: Network
  -> Text 
  -> Credential
  -> Maybe DerivationInfo
  -> OptionsUTxO 
  -> ProposalClose
optionsUTxOToProposalClose network alias stakeCredential mKeyInfo u@OptionsUTxO{..} = 
  ProposalClose
    { utxoRef = utxoRef
    , writerCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , proposalDatum = optionsUTxOProposalDatum u
    , walletAlias = alias
    , network = network
    }
