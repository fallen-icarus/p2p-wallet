module P2PWallet.GUI.EventHandler.OptionsEvent.ResearchEvent
  ( 
    handleResearchEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

handleResearchEvent :: AppModel -> OptionsResearchEvent -> [AppEventResponse AppModel AppEvent]
handleResearchEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetResearchAllActivesFilters -> 
    [ Model $ model 
        & #optionsModel % #researchModel % #activesFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Set the new desired active contract assets
  -----------------------------------------------
  SetResearchActiveContractAssets modal -> case modal of
    StartAdding _ ->
      let oldPair = optionsModel ^. #researchModel % #selectedActiveContractAssets
          newPairInfo = flip (maybe ("","")) oldPair $ \(offerAsset,askAsset) ->
            ( showAssetNameOnly reverseTickerMap $ toNativeAsset offerAsset
            , showAssetNameOnly reverseTickerMap $ toNativeAsset askAsset
            )
       in [ Model $ model
              & #optionsModel % #researchModel % #newActiveContractAssets .~ newPairInfo
              & #optionsModel % #researchModel % #choosingActiveContractAssets .~ True
          ]
    CancelAdding ->
      [ Model $ model
          & #optionsModel % #researchModel % #newActiveContractAssets .~ ("","")
          & #optionsModel % #researchModel % #choosingActiveContractAssets .~ False
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (OptionsEvent . OptionsResearchEvent . SetResearchActiveContractAssets . AddResult) $ do
          let newAssets = optionsModel ^. #researchModel % #newActiveContractAssets
          fromRightOrAppError $ verifyActiveContractAssets tickerMap newAssets
      ]
    AddResult targetAssets ->
      [ Model $ model 
          & #optionsModel % #researchModel % #choosingActiveContractAssets .~ False
          & #optionsModel % #researchModel % #newActiveContractAssets .~ ("","")
          & #optionsModel % #researchModel % #selectedActiveContractAssets ?~ targetAssets
      , Task $ do
          -- Only sync the contracts for the new assets if it has not been synced yet.
          -- Users can manually resync if necessary.
          return $ case Map.lookup targetAssets $ optionsModel ^. #cachedActiveContracts of
            Nothing -> OptionsEvent $ SyncActiveOptionsContracts $ StartProcess $ Just targetAssets
            Just _ -> AppInit
      ]
