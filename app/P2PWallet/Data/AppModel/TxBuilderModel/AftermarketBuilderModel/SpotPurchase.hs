{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotPurchase where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Spot Purchase
-------------------------------------------------
-- | Information for purchasing a spot sale.
data SpotPurchase = SpotPurchase
  -- | The sale being purchased.
  { saleUTxO :: AftermarketUTxO
  -- | The extra minUTxOValue amount of ada required for the new payment UTxO.
  , extraPaymentDeposit :: Lovelace
  -- | Which network the contracts are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SpotPurchase

instance Default SpotPurchase where
  def = SpotPurchase
    { saleUTxO = def
    , extraPaymentDeposit = 0
    , network = def
    }

instance AssetBalancesForChange (a,SpotPurchase) where
  assetBalancesForChange xs =
      ( negate $ sum $ for xs $ \(_, SpotPurchase{..}) ->
          let paymentLoves = maybe 0 (Lovelace . view #quantity)
                           $ maybeHead
                           $ filter ((== "") . view #policyId)
                           $ utxoPrice saleUTxO
           in extraPaymentDeposit + paymentLoves
      , sumNativeAssets $ concat $
          for xs $ \(_,SpotPurchase{..}) ->
            let paymentAssets = filter ((/= "") . view #policyId) $ utxoPrice saleUTxO
             in sumNativeAssets $ concat
                  -- Need the payment amount.
                  [ map (over #quantity negate) paymentAssets
                  -- The NFTs are free.
                  , filter ((/= Aftermarket.beaconCurrencySymbol) . view #policyId) $ 
                      saleUTxO ^. #nativeAssets
                  ]
      )
    where
      utxoPrice :: AftermarketUTxO -> [NativeAsset]
      utxoPrice = maybe [] (map toNativeAsset . Aftermarket.unPrices) . aftermarketUTxOSellerPrice

aftermarketUTxOToSpotPurchase :: Network -> AftermarketUTxO -> SpotPurchase
aftermarketUTxOToSpotPurchase network utxo = SpotPurchase
  { saleUTxO = utxo
  , extraPaymentDeposit = 0
  , network = network
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateSpotPurchaseDeposit :: SpotPurchase -> Lovelace -> SpotPurchase
updateSpotPurchaseDeposit i@SpotPurchase{saleUTxO} calculatedDeposit
  | calculatedDeposit > actualDeposit = i & #extraPaymentDeposit .~ calculatedDeposit - actualDeposit
  | otherwise = i & #extraPaymentDeposit .~ 0
  where
    actualDeposit = maybe 0 Lovelace $ aftermarketUTxOSellerDeposit saleUTxO

-- | Generate the deposit message.
createSpotPurchaseDepositMsg :: SpotPurchase -> Text
createSpotPurchaseDepositMsg SpotPurchase{extraPaymentDeposit}
  | extraPaymentDeposit <= 0 = ""
  | otherwise = unwords
      [ "The new payment UTxO requires an extra"
      , display extraPaymentDeposit
      , "for the deposit. You will need to cover this in addition to the sale price."
      ]
