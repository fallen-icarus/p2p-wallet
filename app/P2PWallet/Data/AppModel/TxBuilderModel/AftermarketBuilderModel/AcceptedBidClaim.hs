{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.AcceptedBidClaim where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Accepted Bid Claim
-------------------------------------------------
-- | Information for claiming a bid that was accepted by the seller.
data AcceptedBidClaim = AcceptedBidClaim
  -- | The bid being claimed.
  { bidUTxO :: AftermarketUTxO
  -- | The extra minUTxOValue amount of ada required for the new payment UTxO.
  , extraPaymentDeposit :: Lovelace
  -- | The market wallet for this buyer.
  , marketWallet :: MarketWallet
  -- | Which network the contracts are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AcceptedBidClaim

instance Default AcceptedBidClaim where
  def = AcceptedBidClaim
    { bidUTxO = def
    , extraPaymentDeposit = 0
    , marketWallet = def
    , network = def
    }

instance AssetBalancesForChange (a,AcceptedBidClaim) where
  assetBalancesForChange xs =
      ( negate $ sum $ for xs $ \(_, AcceptedBidClaim{..}) ->
          let paymentLoves = maybe 0 (Lovelace . view #quantity)
                           $ maybeHead
                           $ filter ((== "") . view #policyId)
                           $ utxoPrice bidUTxO
              bidderDeposit = maybe 0 Lovelace $ aftermarketUTxOBuyerDeposit bidUTxO
           in extraPaymentDeposit + paymentLoves - bidderDeposit -- the deposit can be used
      , sumNativeAssets $ concat $
          for xs $ \(_,AcceptedBidClaim{..}) ->
            let paymentAssets = filter ((/= "") . view #policyId) $ utxoPrice bidUTxO
             in sumNativeAssets $ concat
                  -- Need the payment amount.
                  [ map (over #quantity negate) paymentAssets
                  -- The NFTs are free.
                  , filter ((/= Aftermarket.beaconCurrencySymbol) . view #policyId) $ 
                      bidUTxO ^. #nativeAssets
                  ]
      )
    where
      utxoPrice :: AftermarketUTxO -> [NativeAsset]
      utxoPrice = maybe [] (map toNativeAsset . Aftermarket.unPrices) . aftermarketUTxOBuyerPrice

aftermarketUTxOToAcceptedBidClaim 
  :: Network 
  -> MarketWallet 
  -> AftermarketUTxO 
  -> AcceptedBidClaim
aftermarketUTxOToAcceptedBidClaim network marketWallet utxo = AcceptedBidClaim
  { bidUTxO = utxo
  , extraPaymentDeposit = 0
  , network = network
  , marketWallet = marketWallet
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateAcceptedBidClaimDeposit :: AcceptedBidClaim -> Lovelace -> AcceptedBidClaim
updateAcceptedBidClaimDeposit i@AcceptedBidClaim{bidUTxO} calculatedDeposit
  | calculatedDeposit > actualDeposit = i & #extraPaymentDeposit .~ calculatedDeposit - actualDeposit
  | otherwise = i & #extraPaymentDeposit .~ 0
  where
    bidAmount = map toNativeAsset . view #unPrices <$> aftermarketUTxOBuyerPrice bidUTxO
    lovelaceBid = bidAmount >>= find ((=="") . view #policyId)
    sellerDeposit = maybe 0 Lovelace $ aftermarketUTxOSellerDeposit bidUTxO
    actualDeposit = sellerDeposit + maybe 0 (Lovelace . view #quantity) lovelaceBid

-- | Generate the deposit message.
createAcceptedBidClaimDepositMsg :: AcceptedBidClaim -> Text
createAcceptedBidClaimDepositMsg AcceptedBidClaim{extraPaymentDeposit}
  | extraPaymentDeposit <= 0 = ""
  | otherwise = unwords
      [ "The new payment UTxO requires an extra"
      , display extraPaymentDeposit
      , "for the minUTxOValue. You will need to cover this in addition to the bid payment."
      ]
