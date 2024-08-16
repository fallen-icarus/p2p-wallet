{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.LendingModel.LoanOfferConfiguration where

import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Offer Configuration
-------------------------------------------------
-- | The kind of offer the user is looking for.
data LoanOfferConfiguration = LoanOfferConfiguration
  { lenderCredential :: Maybe Credential
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''LoanOfferConfiguration

instance Default LoanOfferConfiguration where
  def = LoanOfferConfiguration
    { lenderCredential = Nothing
    }

