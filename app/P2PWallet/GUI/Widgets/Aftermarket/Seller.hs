module P2PWallet.GUI.Widgets.Aftermarket.Seller
  ( sellerWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Seller.OpenSales
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests (inspectLoanWidget, inspectBorrowerWidget)
import P2PWallet.Prelude

sellerWidget :: AppModel -> AppNode
sellerWidget model@AppModel{aftermarketModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH sellerSceneMenu
          , spacer
          , box_ [mergeRequired reqUpdate] (openSalesWidget model)
              `nodeVisible` (aftermarketModel ^. #sellerModel % #scene == OpenAftermarketSales)
          -- , box_ [mergeRequired reqUpdate] (activeContractsWidget model)
          --     `nodeVisible` (optionsModel ^. #writerModel % #scene == ActiveOptionsContracts)
          -- , box_ [mergeRequired reqUpdate] (transactionsWidget model)
          --     `nodeVisible` (optionsModel ^. #writerModel % #scene == OptionsTransactions)
          ]
      , inspectSaleWidget model
          `nodeVisible` and
            [ isJust $ aftermarketModel ^. #sellerModel % #inspectedSale
            -- Hide until after syncing is complete.
            , not $ model ^. #waitingStatus % #syncingLoanHistories
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      , widgetMaybe (aftermarketModel ^. #sellerModel % #inspectedBorrower) $ \info ->
          let closeEvt = AftermarketEvent $ AftermarketSellerEvent CloseInspectedSellerBorrowerInformation
              historyEvt = AftermarketEvent . AftermarketSellerEvent . InspectSellerLoanHistory
           in inspectBorrowerWidget info closeEvt historyEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingBorrowerInfo
                  ]
      , widgetMaybe (aftermarketModel ^. #sellerModel % #inspectedLoan) $ \targetId ->
          let closeEvt = AftermarketEvent $ AftermarketSellerEvent CloseInspectedSellerLoanHistory in
          inspectLoanWidget targetId closeEvt model
            `nodeVisible` and
              [ -- Hide until after syncing is complete.
                not $ model ^. #waitingStatus % #syncingLoanHistories
              ]
      ]
  where
    sellerSceneButton :: Text -> AftermarketSellerScene -> AppNode
    sellerSceneButton caption scene = do
      let dormantColor
            | aftermarketModel ^. #sellerModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | aftermarketModel ^. #sellerModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (AftermarketEvent $ AftermarketSellerEvent $ ChangeAftermarketSellerScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    sellerSceneMenu :: AppNode
    sellerSceneMenu = hstack 
      [ spacer
      , sellerSceneButton "Open Sales" OpenAftermarketSales
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , sellerSceneButton "Current Bids" CurrentAftermarketBids
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , sellerSceneButton "Aftermarket History" AftermarketTransactions
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{aftermarketModel=oldMarket} new@AppModel{aftermarketModel=newMarket} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | old ^. #waitingStatus % #syncingLoanHistories 
      /= new ^. #waitingStatus % #syncingLoanHistories = True
      | old ^. #waitingStatus % #syncingOptionsContracts 
      /= new ^. #waitingStatus % #syncingOptionsContracts = True
      | old ^. #waitingStatus % #syncingBorrowerInfo 
      /= new ^. #waitingStatus % #syncingBorrowerInfo = True
      | old ^. #lendingModel % #cachedBorrowerInfo
      /= new ^. #lendingModel % #cachedBorrowerInfo = True
      -- | oldOptions ^. #writerModel % #proposalsFilterModel /=
      --   newOptions ^. #writerModel % #proposalsFilterModel =
      --     openProposalsFilterModelRequiresUpdate
      --         (oldOptions ^. #writerModel % #proposalsFilterModel)
      --         (newOptions ^. #writerModel % #proposalsFilterModel)
      -- | oldOptions ^. #writerModel % #txFilterModel /=
      --   newOptions ^. #writerModel % #txFilterModel =
      --     txFilterModelRequiresUpdate
      --         (oldOptions ^. #writerModel % #txFilterModel)
      --         (newOptions ^. #writerModel % #txFilterModel)
      | otherwise = oldMarket /= newMarket

