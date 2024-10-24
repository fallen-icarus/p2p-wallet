module P2PWallet.GUI.Widgets.Options.Writer
  ( writerWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Options.Writer.ActiveContracts
import P2PWallet.GUI.Widgets.Options.Writer.OpenProposals
import P2PWallet.GUI.Widgets.Options.Writer.Transactions
import P2PWallet.Prelude

writerWidget :: AppModel -> AppNode
writerWidget model@AppModel{optionsModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH writerSceneMenu
          , spacer
          , box_ [mergeRequired reqUpdate] (openProposalsWidget model)
              `nodeVisible` (optionsModel ^. #writerModel % #scene == OpenOptionsProposals)
          , box_ [mergeRequired reqUpdate] (activeContractsWidget model)
              `nodeVisible` (optionsModel ^. #writerModel % #scene == ActiveOptionsContracts)
          , box_ [mergeRequired reqUpdate] (transactionsWidget model)
              `nodeVisible` (optionsModel ^. #writerModel % #scene == OptionsTransactions)
          ]
      , widgetIf (isJust $ optionsModel ^. #writerModel % #inspectedTransaction) $
          inspectionWidget model
      ]
  where
    writerSceneButton :: Text -> OptionsWriterScene -> AppNode
    writerSceneButton caption scene = do
      let dormantColor
            | optionsModel ^. #writerModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | optionsModel ^. #writerModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (OptionsEvent $ OptionsWriterEvent $ ChangeOptionsWriterScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    writerSceneMenu :: AppNode
    writerSceneMenu = hstack 
      [ spacer
      , writerSceneButton "Open Proposals" OpenOptionsProposals
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , writerSceneButton "Active Contracts" ActiveOptionsContracts
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , writerSceneButton "Options History" OptionsTransactions
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{optionsModel=oldOptions} new@AppModel{optionsModel=newOptions} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | oldOptions ^. #writerModel % #proposalsFilterModel /=
        newOptions ^. #writerModel % #proposalsFilterModel =
          openProposalsFilterModelRequiresUpdate
              (oldOptions ^. #writerModel % #proposalsFilterModel)
              (newOptions ^. #writerModel % #proposalsFilterModel)
      | oldOptions ^. #writerModel % #txFilterModel /=
        newOptions ^. #writerModel % #txFilterModel =
          txFilterModelRequiresUpdate
              (oldOptions ^. #writerModel % #txFilterModel)
              (newOptions ^. #writerModel % #txFilterModel)
      | otherwise = oldOptions /= newOptions

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
openProposalsFilterModelRequiresUpdate 
  :: OpenProposalsFilterModel 
  -> OpenProposalsFilterModel 
  -> Bool
openProposalsFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #shouldBeExpired /= new ^. #shouldBeExpired = True
  | old ^. #offerAsset /= new ^. #offerAsset = False
  | old ^. #askAsset /= new ^. #askAsset = False
  | old ^. #premiumAsset /= new ^. #premiumAsset = False
  | otherwise = old /= new

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
txFilterModelRequiresUpdate 
  :: OptionsTxFilterModel 
  -> OptionsTxFilterModel 
  -> Bool
txFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #offerAsset /= new ^. #offerAsset = False
  | old ^. #askAsset /= new ^. #askAsset = False
  | old ^. #premiumAsset /= new ^. #premiumAsset = False
  | otherwise = old /= new
