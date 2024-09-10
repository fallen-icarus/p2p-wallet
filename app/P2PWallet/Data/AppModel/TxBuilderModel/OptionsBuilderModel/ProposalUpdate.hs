{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalUpdate where

import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time

import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalClose
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalCreation
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Proposal Update
-------------------------------------------------
-- | An proposal update is just the composition of closing one proposal and creating another. Whether
-- beacons need to be changed depends on the exact composition.
data ProposalUpdate = ProposalUpdate
  { oldProposal :: ProposalClose
  , newProposal :: ProposalCreation
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ProposalUpdate

instance AssetBalancesForChange (a,ProposalUpdate) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #oldProposal) xs
    , assetBalancesForChange $ map (over _2 $ view #newProposal) xs
    ]

-- | Create a populated `NewProposalCreation` based on the current proposal terms.
optionsUTxOToNewProposalCreation 
  :: Network 
  -> Text 
  -> ReverseTickerMap 
  -- | Either the first wallet in the tracked payment wallet list or the
  -- payment wallet already associated with the offer.
  -> PaymentWallet 
  -> TimeZone
  -> OptionsUTxO 
  -> NewProposalCreation
optionsUTxOToNewProposalCreation network alias reverseTickerMap wallet timeZone u = 
    NewProposalCreation
      { optionsAddress = optionsAddress
      , offerAsset = showAssetBalance True reverseTickerMap offerAmount
      , askAsset = showAssetNameOnly reverseTickerMap askAssetAsNativeAsset
      , premiumAsset = showAssetNameOnly reverseTickerMap premiumAssetAsNativeAsset
      , possibleTerms = unlines $ map showTerms possibleTerms
      , paymentWallet = wallet
      , count = 1
      , network = network
      , alias = alias
      }
  where
    OptionsUTxO{..} = u

    Options.ProposalDatum{..} = fromMaybe def $ optionsUTxOProposalDatum u
    offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
    premiumAssetAsNativeAsset = toNativeAsset premiumAsset
    askAssetAsNativeAsset = toNativeAsset askAsset

    showTerms :: Options.Terms -> Text
    showTerms Options.Terms{premium, strikePrice, expiration} = mconcat $ intersperse ", "
      [ showAssetQuantityOnly reverseTickerMap $ premiumAssetAsNativeAsset & #quantity .~ premium
      , showPriceFormatted reverseTickerMap askAssetAsNativeAsset offerAmount $ toRational strikePrice
      -- Midnight is technically the next day so must subtract 1 second.
      , showDate $ fromPlutusTime $ expiration - 1 
      ]

    showDate :: POSIXTime -> Text
    showDate t = toText $ Time.formatTime locale formatter localTime
      where
        locale = Time.defaultTimeLocale & #knownTimeZones %~ flip snoc timeZone
        utcTime = Time.posixSecondsToUTCTime t
        localTime = Time.utcToLocalTime timeZone utcTime
        formatter = "%m/%d/%y"
