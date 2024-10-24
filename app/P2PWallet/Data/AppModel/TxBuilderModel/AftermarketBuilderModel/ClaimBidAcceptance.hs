{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.ClaimBidAcceptance where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Claim Bid Acceptance
-------------------------------------------------
-- | Information for accepting a claim bid.
data ClaimBidAcceptance = ClaimBidAcceptance
  -- | The bid being accepted.
  { bidUTxO :: AftermarketUTxO
  -- | The extra minUTxOValue amount of ada required for the new AcceptedBid UTxO.
  , extraDeposit :: Lovelace
  -- | The address where the bid payment must go.
  , paymentWallet :: PaymentWallet
  -- | The market wallet for this seller
  , marketWallet :: MarketWallet
  -- | Which network the contracts are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ClaimBidAcceptance

instance Default ClaimBidAcceptance where
  def = ClaimBidAcceptance
    { bidUTxO = def
    , extraDeposit = 0
    , network = def
    , currentTime = 0
    , paymentWallet = def
    , marketWallet = def
    }

instance AssetBalancesForChange (a,ClaimBidAcceptance) where
  assetBalancesForChange xs =
    ( negate $ sum $ for xs $ \(_, ClaimBidAcceptance{..}) -> extraDeposit
    , sumNativeAssets $ concat $
        for xs $ \(_,ClaimBidAcceptance{..}) ->
          let (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts bidUTxO
           in map (set #quantity (-1) . mkNativeAsset policyId) names -- Need the nfts.
    )

aftermarketUTxOToClaimBidAcceptance 
  :: POSIXTime 
  -> Network 
  -> MarketWallet
  -> PaymentWallet
  -> AftermarketUTxO 
  -> ClaimBidAcceptance
aftermarketUTxOToClaimBidAcceptance currentTime network marketWallet paymentWallet utxo = ClaimBidAcceptance
  { bidUTxO = utxo
  , extraDeposit = 
      -- This will be over-ridden, but is needed to get the datum size correct for the minUTxOValue
      -- calculation. If it was set to zero, inserting this value into the datum will increase its
      -- size and increase the required minUTxOValue. By starting high, the minUTxOValue will
      -- decrease when the actual deposit is inserted into the datum.
      9_999_999
  , network = network
  , currentTime = toPlutusTime currentTime
  , paymentWallet = paymentWallet
  , marketWallet = marketWallet
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateClaimBidAcceptanceDeposit :: ClaimBidAcceptance -> Lovelace -> ClaimBidAcceptance
updateClaimBidAcceptanceDeposit i@ClaimBidAcceptance{bidUTxO} calculatedDeposit
  | calculatedDeposit > actualDeposit = i & #extraDeposit .~ calculatedDeposit - actualDeposit
  | otherwise = i & #extraDeposit .~ 0
  where
    actualDeposit = bidUTxO ^. #lovelace

-- | Generate the deposit message.
createClaimBidAcceptanceDepositMsg :: ClaimBidAcceptance -> Text
createClaimBidAcceptanceDepositMsg ClaimBidAcceptance{extraDeposit}
  | extraDeposit <= 0 = ""
  | otherwise = unwords
      [ "The new accepted bid UTxO requires an extra"
      , display extraDeposit
      , "for the deposit. You will get this back when the bidder claims the NFTs."
      ]
