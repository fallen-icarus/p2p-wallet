{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.AskClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Ask Close
-------------------------------------------------
-- | Information for closing an ask.
data AskClose = AskClose
  { utxoRef :: TxOutRef
  -- | The stake credential for this borrower.
  , borrowerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current ask terms.
  , askDatum :: Maybe AskDatum
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AskClose

instance AssetBalancesForChange (a,AskClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= negotiationBeaconCurrencySymbol

loanUTxOToAskClose 
  :: Network
  -> Text 
  -> Credential
  -> Maybe DerivationInfo
  -> LoanUTxO 
  -> AskClose
loanUTxOToAskClose network alias stakeCredential mKeyInfo u@LoanUTxO{..} = 
  AskClose
    { utxoRef = utxoRef
    , borrowerCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , askDatum = loanUTxOAskDatum u
    , walletAlias = alias
    , network = network
    }
