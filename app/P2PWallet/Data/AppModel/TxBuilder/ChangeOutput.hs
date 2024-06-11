{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilder.ChangeOutput where

import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Prelude

-------------------------------------------------
-- Change Output
-------------------------------------------------
-- | Information for the change output. The fee will be deducted from this output.
data ChangeOutput = ChangeOutput
  -- | The address where the change will go.
  { paymentAddress :: PaymentAddress
  -- | The amount of ada to be returned as change.
  , lovelace :: Lovelace
  -- | The native assets quantities to be returned as change.
  , nativeAssets :: [NativeAsset]
  -- | Whether the widget expands the info for this output.
  , showDetails :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ChangeOutput

