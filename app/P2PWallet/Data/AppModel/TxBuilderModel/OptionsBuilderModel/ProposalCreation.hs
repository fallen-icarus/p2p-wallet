{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalCreation where

import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Proposal Creation
-------------------------------------------------
-- | Information for a new options proposal.
data ProposalCreation = ProposalCreation
  -- | The target bech32 address for the new proposal.
  { optionsAddress :: PaymentAddress
  -- | The asset being offered.
  , offerAsset :: NativeAsset
  -- | The asset being asked for.
  , askAsset :: NativeAsset
  -- | The asset that must be used for the premium payment.
  , premiumAsset :: NativeAsset
  -- | The address where the premium payment and execution payment must go.
  , paymentWallet :: PaymentWallet
  -- | The possible terms.
  , possibleTerms :: [(NativeAsset, Rational, POSIXTime)]
  -- | The number of desired new asks with these details.
  , count :: Int
  -- | The amount of ada used for the min UTxO value. 
  , deposit :: Lovelace
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the borrower credential.
  , alias :: Text
  -- | Current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ProposalCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,ProposalCreation) where
  assetBalancesForChange xs =
    ( sum $ for xs $ \(_,ProposalCreation{count,offerAsset,deposit}) -> 
        -- The total amount of ada is the deposit + the offer amount if ada is the offered
        -- asset.
        negate $ case offerAsset ^. #policyId of
          "" -> fromIntegral count * (deposit + Lovelace (offerAsset ^. #quantity))
          _ -> fromIntegral count * deposit
    , flip concatMap xs $ \(_,ProposalCreation{offerAsset,count}) -> 
        -- Native assets are only required for the offer amount itself.
        case offerAsset ^. #policyId of
          "" -> []
          _ -> [offerAsset & #quantity %~ (negate . (* fromIntegral count))]
    )

-------------------------------------------------
-- New Proposal Creation
-------------------------------------------------
-- | Information from the user that will be verified and converted to a `ProposalCreation`.
data NewProposalCreation = NewProposalCreation
  -- | The target bech32 address for the new proposal.
  { optionsAddress :: PaymentAddress
  -- | The asset being offered.
  , offerAsset :: Text
  -- | The asset being asked for.
  , askAsset :: Text
  -- | The asset that must be used for the premium payment.
  , premiumAsset :: Text
  -- | The address where the premium payment and execution payment must go.
  , paymentWallet :: PaymentWallet
  -- | The possible terms. The terms must be separated with newlines.
  , possibleTerms :: Text
  -- | This is used internally to preserve the current count when converting back from AskCreation.
  , count :: Int
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the options credential.
  , alias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewProposalCreation

instance Default NewProposalCreation where
  def = NewProposalCreation
    { optionsAddress = ""
    , offerAsset = ""
    , askAsset = ""
    , premiumAsset = ""
    , paymentWallet = def
    , possibleTerms = ""
    , count = 1
    , network = def
    , alias = ""
    }

-- | Create a fresh `NewProposalCreation`.
createNewProposalCreation 
  :: Network 
  -> OptionsWallet 
  -> PaymentWallet
  -> NewProposalCreation
createNewProposalCreation network optionsWallet paymentWallet =
  def & #network .~ network
      & #optionsAddress .~ optionsWallet ^. #optionsAddress
      & #alias .~ optionsWallet ^. #alias
      & #paymentWallet .~ paymentWallet

-------------------------------------------------
-- NewProposalCreation <--> ProposalCreation
-------------------------------------------------
-- | Verify the user info for the new proposal.
verifyNewProposalCreation 
  :: ReverseTickerMap 
  -> TickerMap 
  -> TimeZone
  -> POSIXTime
  -> NewProposalCreation 
  -> Either Text ProposalCreation
verifyNewProposalCreation reverseTickerMap tickerMap timeZone currentTime NewProposalCreation{..} = do
    -- Check that the offer asset is valid. No fingerprints can be used.
    verifiedOfferAmount <-
      first (const $ parseAmountErrorMsg offerAsset) $ parseNativeAssets tickerMap mempty offerAsset

    -- Check that the ask asset is valid. No fingerprints can be used.
    verifiedAskAsset <- parseNativeAssetName tickerMap askAsset

    -- Check that the premium asset is valid. No fingerprints can be used.
    verifiedPremiumAsset <- parseNativeAssetName tickerMap premiumAsset

    verifiedPossibleTerms <-
      if "" == possibleTerms then
        Left "Possible terms cannot be empty."
      else
        mapM (parseTerms verifiedPremiumAsset verifiedOfferAmount verifiedAskAsset) $ 
          lines possibleTerms

    return $ ProposalCreation
      { optionsAddress = optionsAddress
      , offerAsset = verifiedOfferAmount
      , askAsset = verifiedAskAsset
      , premiumAsset = verifiedPremiumAsset
      , paymentWallet = paymentWallet
      , possibleTerms = verifiedPossibleTerms
      , count = 1
      , deposit = 
          -- This is just a placeholder since the minUTxOValue depends on the size of the
          -- datum. This will be replaced by the actual value calculated for the minUTxOValue.
          9_999_999
      , network = network
      , alias = alias
      , currentTime = toPlutusTime currentTime
      }
  where
    -- A custom error message is used since fingerprints are not allowed.
    parseAmountErrorMsg :: Text -> Text
    parseAmountErrorMsg x = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# ticker'"
      , ""
      , "Could not parse: '" <> x <> "'"
      , ""
      , "If using a ticker, make sure it is in the Ticker Registry."
      ]

    -- The expiration is the end of the specified day (midnight).
    parseDate :: Text -> Either Text POSIXTime
    parseDate t = 
      let locale = Time.defaultTimeLocale & #knownTimeZones %~ flip snoc timeZone
          dayToPosixTime = localTimeToPosixTime timeZone . endOfDay
       in maybeToRight ("Could not parse date: " <> t) $ 
            dayToPosixTime <$> (Time.parseTimeM True locale "%m/%d/%y" $ toString t :: Maybe Day)

    parseTerms 
      :: NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Text 
      -> Either Text (NativeAsset, Rational, POSIXTime)
    parseTerms vPremiumAsset vOfferAsset vAskAsset t = case words $ T.replace "," " " t of
      [premium, strikePrice, expiration] -> do
        verifiedPremium <- parseFormattedAssetQuantity reverseTickerMap vPremiumAsset premium
        verifiedPrice <- parseFormattedPrice reverseTickerMap vAskAsset vOfferAsset strikePrice
        -- The expiration is the end of the specified day (midnight).
        verifiedExpiration <- parseDate expiration
        return (verifiedPremium, verifiedPrice, verifiedExpiration)
      _ -> Left $ "Could not parse: " <> t

toNewProposalCreation
  :: ReverseTickerMap
  -> TimeZone
  -> ProposalCreation
  -> NewProposalCreation
toNewProposalCreation reverseTickerMap timeZone ProposalCreation{..} = NewProposalCreation
    { optionsAddress = optionsAddress
    , offerAsset = showAssetBalance True reverseTickerMap offerAsset
    , premiumAsset = showAssetNameOnly reverseTickerMap premiumAsset
    , askAsset = showAssetNameOnly reverseTickerMap askAsset
    , paymentWallet = paymentWallet
    , possibleTerms = unlines $ map showTerms possibleTerms
    , count = count
    , network = network
    , alias = alias
    }
  where
    showTerms :: (NativeAsset, Rational, POSIXTime) -> Text
    showTerms (premium, strikePrice, expiration) = mconcat $ intersperse ", "
      [ showAssetQuantityOnly reverseTickerMap premium
      , showPriceFormatted reverseTickerMap askAsset offerAsset strikePrice
      -- Midnight is technically the next day so must subtract 1 second.
      , showDate $ expiration - 1 
      ]

    showDate :: POSIXTime -> Text
    showDate t = toText $ Time.formatTime locale formatter localTime
      where
        locale = Time.defaultTimeLocale & #knownTimeZones %~ flip snoc timeZone
        utcTime = Time.posixSecondsToUTCTime t
        localTime = Time.utcToLocalTime timeZone utcTime
        formatter = "%m/%d/%y"
