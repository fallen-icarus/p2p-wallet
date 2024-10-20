module P2PWallet.GUI.Widgets.Aftermarket.Seller
  ( sellerWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Seller.CurrentBids
import P2PWallet.GUI.Widgets.Aftermarket.Seller.OpenSales
import P2PWallet.GUI.Widgets.Aftermarket.Seller.TransactionHistory
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
          , box_ [mergeRequired reqUpdate] (currentBidsWidget model)
              `nodeVisible` (aftermarketModel ^. #sellerModel % #scene == CurrentAftermarketBids)
          , box_ [mergeRequired reqUpdate] (transactionsWidget model)
              `nodeVisible` (aftermarketModel ^. #sellerModel % #scene == AftermarketTransactions)
          ]
      , widgetIf (isJust $ aftermarketModel ^. #sellerModel % #inspectedTransaction) $
          txInspectionWidget model
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
      , sellerSceneButton "Store History" AftermarketTransactions
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

      | oldMarket ^. #sellerModel % #txFilterModel 
      /= newMarket ^. #sellerModel % #txFilterModel =
          txHistoryRequiresUpdate
              (oldMarket ^. #sellerModel % #txFilterModel)
              (newMarket ^. #sellerModel % #txFilterModel)

      | otherwise = oldMarket /= newMarket

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
txHistoryRequiresUpdate :: SellerTxFilterModel -> SellerTxFilterModel -> Bool
txHistoryRequiresUpdate old new
  | old ^. #policyId /= new ^. #policyId = False
  | old ^. #dateRange /= new ^. #dateRange = False
  | otherwise = old /= new
