{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Koios.LinkedPaymentAddresses where

import Data.Aeson
import Data.Vector (Vector)

import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Prelude

newtype LinkedPaymentAddresses = 
  LinkedPaymentAddresses { unLinkedPaymentAddresses :: [PaymentAddress] }

instance FromJSON LinkedPaymentAddresses where
  parseJSON = withArray "LinkedPaymentAddresses"
            $ fmap (LinkedPaymentAddresses . concat . toList)
            . mapM (fmap (toList @Vector) . withObject "InnerLinkedPaymentAddressesObject" (.: "addresses"))
