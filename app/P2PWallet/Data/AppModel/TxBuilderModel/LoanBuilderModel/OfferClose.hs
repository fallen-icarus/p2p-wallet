{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.OfferClose where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Close
-------------------------------------------------
-- | Information for closing an offer.
data OfferClose = OfferClose
  { utxoRef :: TxOutRef
  -- | The stake credential for this lender.
  , lenderCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The amount of ada in this UTxO.
  , lovelace :: Lovelace
  -- | The native assets in this UTxO.
  , nativeAssets :: [NativeAsset]
  -- | The current offer terms.
  , offerDatum :: Maybe OfferDatum
  -- | Wallet this offer UTxO is for.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OfferClose

instance AssetBalancesForChange (a,OfferClose) where
  assetBalancesForChange xs =
      ( sum $ map (view #lovelace . snd) xs
      , filterOutBeacons $ concatMap (view #nativeAssets . snd) xs
      )
    where
      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= negotiationBeaconCurrencySymbol

loanUTxOToOfferClose 
  :: Network
  -> Text 
  -> Credential
  -> Maybe DerivationInfo
  -> LoanUTxO 
  -> OfferClose
loanUTxOToOfferClose network alias stakeCredential mKeyInfo u@LoanUTxO{..} = 
  OfferClose
    { utxoRef = utxoRef
    , lenderCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , lovelace = lovelace
    , nativeAssets = nativeAssets
    , offerDatum = loanUTxOOfferDatum u
    , walletAlias = alias
    , network = network
    }
