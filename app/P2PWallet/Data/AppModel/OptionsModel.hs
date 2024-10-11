{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Options scene is dedicated to Cardano-Options `OptionsWallet`.

-}
module P2PWallet.Data.AppModel.OptionsModel 
  ( OptionsScene(..)
  , OptionsEvent(..)
  , OptionsModel(..)

  , CachedOptionsProposals
  , CachedKeyOptionsContracts
  , CachedActiveOptionsContracts

  , verifyProposalContractAssets
  , verifyActiveContractAssets

  , module P2PWallet.Data.AppModel.OptionsModel.BuyerModel
  , module P2PWallet.Data.AppModel.OptionsModel.ResearchModel
  , module P2PWallet.Data.AppModel.OptionsModel.WriterModel
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.OptionsModel.BuyerModel
import P2PWallet.Data.AppModel.OptionsModel.ResearchModel
import P2PWallet.Data.AppModel.OptionsModel.WriterModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Options Proposals
-------------------------------------------------
-- | A type alias for a map from trading pair to known proposals.
type CachedOptionsProposals = Map.Map (OfferAsset, AskAsset, Maybe PremiumAsset) [OptionsUTxO]

-------------------------------------------------
-- Cached Options Contracts for the associated key NFTs
-------------------------------------------------
-- | A type alias for a map from Key NFT to contract state.
type CachedKeyOptionsContracts = Map.Map ContractId (Maybe OptionsUTxO)

-------------------------------------------------
-- Cached Active Options Contracts
-------------------------------------------------
-- | A type alias for a map from trading pair to active options contracts.
type CachedActiveOptionsContracts = Map.Map (OfferAsset, AskAsset) [OptionsUTxO]

-------------------------------------------------
-- Options Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Options page.
data OptionsScene
  -- | Information for the staking credential used as an options' writer.
  = OptionsWriterScene
  -- | Information for the staking credential used as a lender.
  | OptionsBuyerScene
  -- | Research options.
  | OptionsResearchScene
  deriving (Eq,Show)

-------------------------------------------------
-- Options Page Events
-------------------------------------------------
-- | The possible UI events on the Options page.
data OptionsEvent
  -- | Change the Options scene to the specified scene.
  = ChangeOptionsScene OptionsScene
  -- | Add a new options wallet using one of the known staking credentials.
  | AddNewOptionsWallet (AddEvent OptionsWallet OptionsWallet)
  -- | Delete an options wallet.
  | DeleteOptionsWallet (DeleteWithConfirmationEvent OptionsWallet)
  -- | Open the more popup widget
  | ShowOptionsMorePopup
  -- | Sell Options Event.
  | OptionsWriterEvent OptionsWriterEvent
  -- | Buy Options Event.
  | OptionsBuyerEvent OptionsBuyerEvent
  -- | Research Options Event.
  | OptionsResearchEvent OptionsResearchEvent
  -- | Sync the proposals for the selected contract assets.
  | SyncOptionsProposals (ProcessEvent (OfferAsset, AskAsset, Maybe PremiumAsset) CachedOptionsProposals)
  -- | Sync the options contracts for the selected options key nfts.
  | LookupOptionsContracts (ProcessEvent [ContractId] CachedKeyOptionsContracts)
  -- | Sync the active contracts for the selected trading pair.
  | SyncActiveOptionsContracts (ProcessEvent (OfferAsset, AskAsset) CachedActiveOptionsContracts)

-------------------------------------------------
-- Options State
-------------------------------------------------
data OptionsModel = OptionsModel
  -- | The current subscene.
  { scene :: OptionsScene
  -- | The currently focused `OptionsWallet` from the list of tracked `OptionsWallet`s.
  , selectedWallet :: OptionsWallet
  -- | The stake wallet to possibly use for the new options' wallet. This enables previewing
  -- the stake wallet's info before confirming.
  , targetStakeCredential :: Maybe StakeWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The sell model.
  , writerModel :: OptionsWriterModel
  -- | The buy model.
  , buyerModel :: OptionsBuyerModel
  -- | The research model.
  , researchModel :: OptionsResearchModel
  -- | Cached proposal contracts.
  , cachedProposals :: CachedOptionsProposals
  -- | Cached key associated options contracts.
  , cachedKeyContracts :: CachedKeyOptionsContracts
  -- | Cached active options contracts.
  , cachedActiveContracts :: CachedActiveOptionsContracts
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsModel

instance Default OptionsModel where
  def = OptionsModel
    { scene = OptionsWriterScene
    , selectedWallet = def
    , targetStakeCredential = Nothing
    , addingWallet = False
    , deletingWallet = False
    , showMorePopup = False
    , writerModel = def
    , buyerModel = def
    , researchModel = def
    , cachedProposals = mempty
    , cachedKeyContracts = mempty
    , cachedActiveContracts = mempty
    }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Verify the proposal contract assets.
verifyProposalContractAssets 
  :: TickerMap 
  -> (Text,Text,Text) 
  -> Either Text (OfferAsset, AskAsset, Maybe PremiumAsset)
verifyProposalContractAssets tickerMap (rawOffer, rawAsk, rawPremium) = do
  -- Check that the offer asset is valid. No fingerprints can be used.
  verifiedOfferAsset <- fromNativeAsset <$> parseNativeAssetName tickerMap rawOffer

  -- Check that the ask asset is valid. No fingerprints can be used.
  verifiedAskAsset <- fromNativeAsset <$> parseNativeAssetName tickerMap rawAsk

  -- If a premium asset is set, check that the premium asset is valid. 
  -- No fingerprints can be used.
  mVerifiedPremiumAsset <- case rawPremium of
    "" -> return Nothing
    xs -> Just . fromNativeAsset <$> parseNativeAssetName tickerMap xs

  return (verifiedOfferAsset, verifiedAskAsset, mVerifiedPremiumAsset)

-- | Verify the active contract assets.
verifyActiveContractAssets 
  :: TickerMap 
  -> (Text,Text) 
  -> Either Text (OfferAsset, AskAsset)
verifyActiveContractAssets tickerMap (rawOffer, rawAsk) = do
  -- Check that the offer asset is valid. No fingerprints can be used.
  verifiedOfferAsset <- fromNativeAsset <$> parseNativeAssetName tickerMap rawOffer

  -- Check that the ask asset is valid. No fingerprints can be used.
  verifiedAskAsset <- fromNativeAsset <$> parseNativeAssetName tickerMap rawAsk

  return (verifiedOfferAsset, verifiedAskAsset)
