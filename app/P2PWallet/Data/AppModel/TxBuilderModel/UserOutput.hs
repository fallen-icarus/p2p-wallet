{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.UserOutput where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-------------------------------------------------
-- User Outputs
-------------------------------------------------
-- | Information for a particular user output. These are non-defi outputs. 
data UserOutput = UserOutput
  -- | The name of the recipient.
  { alias :: Text
  -- | The target bech32 address.
  , paymentAddress :: PaymentAddress
  -- | The desired amount of ada.
  , lovelace :: Lovelace
  -- | The desired native assets.
  , nativeAssets :: [NativeAsset]
  -- | Whether the widget expands the info for this output.
  , showDetails :: Bool 
  -- | The number of desired outputs with these details.
  , count :: Int
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''UserOutput

instance AddToTxBody UserOutput where
  addToTxBody txBody UserOutput{..} = 
      txBody 
        -- Add one instance of the output per count and preserve ordering of the
        -- output list.
        & #outputs %~ (<> replicate count newOutput)
    where 
      newOutput :: TxBodyOutput
      newOutput = TxBodyOutput
        { paymentAddress = paymentAddress
        , lovelace = lovelace
        , nativeAssets = nativeAssets
        }

-------------------------------------------------
-- New User Outputs
-------------------------------------------------
-- | Information from the user that will be verified and converted to a `UserOutput`.
data NewUserOutput = NewUserOutput
  -- | The name of the recipient.
  { alias :: Text
  -- | The target bech32 address.
  , paymentAddress :: Text
  -- | The desired amount of ada.
  , ada :: Text
  -- | The desired native assets.
  , nativeAssets :: Text
  -- | This is used internally to preserve the current count when converting back from UserOutput.
  , count :: Int
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewUserOutput

instance Default NewUserOutput where
  def = NewUserOutput
    { alias = ""
    , paymentAddress = ""
    , ada = ""
    , nativeAssets = ""
    , count = 1
    }

-------------------------------------------------
-- NewUserOutput <--> UserOutput
-------------------------------------------------
-- | Verify the user info for the new output.
processNewUserOutput 
  :: Network 
  -> TickerMap 
  -> FingerprintMap 
  -> NewUserOutput 
  -> Either Text UserOutput
processNewUserOutput network tickerMap fingerprintMap NewUserOutput{..} = do
  -- Verify the address is a valid address.
  addr <- parsePaymentAddress network paymentAddress

  -- Check that the ada balance is positive. It cannot be zero.
  adaQuantity <- parseAda False ada

  -- Check that the assets are valid. Returns the first error, if any.
  assets <- mapM (parseNativeAssets tickerMap fingerprintMap) $ lines nativeAssets

  -- Create the new output.
  return $ UserOutput
    { alias = alias
    , paymentAddress = addr
    , lovelace = toLovelace adaQuantity
    , nativeAssets = assets
    , showDetails = False
    , count = count
    }

toNewUserOutput :: ReverseTickerMap -> UserOutput -> NewUserOutput
toNewUserOutput reverseTickerMap UserOutput{..} = NewUserOutput
  { alias = alias
  , paymentAddress = toText paymentAddress
  , ada = toText $ toAda lovelace
  , nativeAssets = unlines $ map (showAssetBalance True reverseTickerMap) nativeAssets
  , count = count
  }
