{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ContractExecution where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Execute Contract
-------------------------------------------------
-- | Information for executing an options contract.
data OptionsContractExecution = OptionsContractExecution
  { optionsUTxO :: OptionsUTxO
  -- | Required ask value.
  , askValue :: NativeAsset
  -- | Wallet claiming this UTxO.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The time of the execution.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsContractExecution

instance AssetBalancesForChange (a,OptionsContractExecution) where
  assetBalancesForChange xs = sumAssetBalances
      [ -- Value from optionsUTxO.
        ( sum $ map (view $ _2 % #optionsUTxO % #lovelace) xs
        , normalAssets
        )
      , sumAssetBalances $ map (requiredExtraValue . snd) xs
      ]
    where
      normalAssets :: [NativeAsset]
      normalAssets = filterOutBeacons $ concatMap (view $ _2 % #optionsUTxO % #nativeAssets) xs

      requiredExtraValue :: OptionsContractExecution -> (Lovelace, [NativeAsset])
      requiredExtraValue OptionsContractExecution{..} =
        let Options.ActiveDatum{contractId,contractDeposit} = 
              fromMaybe def $ optionsUTxOActiveDatum optionsUTxO
            contractIdAsset = 
              mkNativeAsset Options.activeBeaconCurrencySymbol (contractId ^. #unContractId) 
            extraContractId = contractIdAsset & #quantity .~ (-1)
            requiredLovelace
              | askValue ^. #policyId == "" = negate 
                                            $ Lovelace 
                                            $ askValue ^. #quantity + contractDeposit
              | otherwise = negate 
                          $ Lovelace contractDeposit
            requiredNativeAssets
              | askValue ^. #policyId /= "" = [extraContractId, askValue & #quantity %~ negate]
              | otherwise = [extraContractId]
         in (requiredLovelace, requiredNativeAssets)

      filterOutBeacons :: [NativeAsset] -> [NativeAsset]
      filterOutBeacons = filter $ \NativeAsset{policyId} -> 
        policyId /= Options.activeBeaconCurrencySymbol

optionsUTxOToOptionsContractExecution
  :: Network
  -> Text
  -> POSIXTime
  -> OptionsUTxO
  -> OptionsContractExecution
optionsUTxOToOptionsContractExecution network alias currentTime optionsUTxO = 
    OptionsContractExecution
      { optionsUTxO = optionsUTxO
      , walletAlias = alias
      , network = network
      , currentTime = toPlutusTime currentTime
      , askValue = askValue
      }
  where
    askValue =
      let Options.ActiveDatum{offerQuantity,askAsset,strikePrice} = 
            fromMaybe def $ optionsUTxOActiveDatum optionsUTxO
          askQuantity = roundUp $ toRational strikePrice * toRational offerQuantity
       in toNativeAsset askAsset & #quantity .~ askQuantity
