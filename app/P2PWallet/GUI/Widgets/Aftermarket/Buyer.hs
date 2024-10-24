module P2PWallet.GUI.Widgets.Aftermarket.Buyer
  ( buyerWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Buyer.Aftermarket
import P2PWallet.GUI.Widgets.Aftermarket.Buyer.BidHistory
import P2PWallet.GUI.Widgets.Aftermarket.Buyer.OwnBids
import P2PWallet.GUI.Widgets.Aftermarket.Common
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests (inspectLoanWidget, inspectBorrowerWidget)
import P2PWallet.Prelude

buyerWidget :: AppModel -> AppNode
buyerWidget model@AppModel{aftermarketModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH buyerSceneMenu
          , spacer
          , box_ [mergeRequired reqUpdate] (aftermarketWidget model)
              `nodeVisible` (aftermarketModel ^. #buyerModel % #scene == PolicyAftermarketSales)
          , box_ [mergeRequired reqUpdate] (ownBidsWidget model)
              `nodeVisible` (aftermarketModel ^. #buyerModel % #scene == OwnAftermarketBids)
          , box_ [mergeRequired reqUpdate] (transactionsWidget model)
              `nodeVisible` (aftermarketModel ^. #buyerModel % #scene == AftermarketBidTransactions)
          ]
      , widgetIf (isJust $ aftermarketModel ^. #buyerModel % #inspectedBidTransaction) $
          bidTxInspectionWidget model
      , widgetMaybe (aftermarketModel ^. #buyerModel % #inspectedSeller) $ \sellerAddr ->
          let closeEvt = AftermarketEvent $ AftermarketBuyerEvent CloseInspectedSellerInformation
              inspectSaleEvt = AftermarketEvent . AftermarketBuyerEvent . InspectAftermarketBuyerSale
              inspectBidEvt = AftermarketEvent . AftermarketBuyerEvent . InspectAftermarketBid
           in inspectSellerWidget sellerAddr closeEvt inspectSaleEvt inspectBidEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingSellerInfo
                  ]
      , widgetMaybe (aftermarketModel ^. #buyerModel % #inspectedSale) $ \saleUTxO ->
          inspectBatchWidget model 
            InspectBatchConfig
              { batchUTxO = saleUTxO
              , closeEvent = 
                  AftermarketEvent $ AftermarketBuyerEvent CloseInspectedAftermarketBuyerSale
              , inspectLoanHistoryEvent =
                  AftermarketEvent . AftermarketBuyerEvent . InspectBuyerLoanHistory
              , lookupBorrowerEvent =
                  AftermarketEvent . AftermarketBuyerEvent . InspectBuyerBorrowerInformation
              , mAddToHomeEvent = Nothing
              }
          `nodeVisible` and
            -- Hide until after syncing is complete.
            [ not $ model ^. #waitingStatus % #syncingLoanHistories
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      , widgetMaybe (aftermarketModel ^. #buyerModel % #inspectedBid) $ \bidUTxO ->
          inspectBatchWidget model 
            InspectBatchConfig
              { batchUTxO = bidUTxO
              , closeEvent = 
                  AftermarketEvent $ AftermarketBuyerEvent CloseInspectedAftermarketBid
              , inspectLoanHistoryEvent =
                  AftermarketEvent . AftermarketBuyerEvent . InspectBuyerLoanHistory
              , lookupBorrowerEvent =
                  AftermarketEvent . AftermarketBuyerEvent . InspectBuyerBorrowerInformation
              , mAddToHomeEvent = Nothing
              }
          `nodeVisible` and
            -- Hide until after syncing is complete.
            [ not $ model ^. #waitingStatus % #syncingLoanHistories
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      , widgetMaybe (aftermarketModel ^. #buyerModel % #inspectedBorrower) $ \info ->
          let closeEvt = AftermarketEvent $ AftermarketBuyerEvent CloseInspectedBuyerBorrowerInformation
              historyEvt = AftermarketEvent . AftermarketBuyerEvent . InspectBuyerLoanHistory
           in inspectBorrowerWidget info closeEvt historyEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingBorrowerInfo
                  ]
      , widgetMaybe (aftermarketModel ^. #buyerModel % #inspectedLoan) $ \targetId ->
          let closeEvt = AftermarketEvent $ AftermarketBuyerEvent CloseInspectedBuyerLoanHistory in
          inspectLoanWidget targetId closeEvt model
            `nodeVisible` and
              [ -- Hide until after syncing is complete.
                not $ model ^. #waitingStatus % #syncingLoanHistories
              ]
      ]
  where
    buyerSceneButton :: Text -> AftermarketBuyerScene -> AppNode
    buyerSceneButton caption scene = do
      let dormantColor
            | aftermarketModel ^. #buyerModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | aftermarketModel ^. #buyerModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (AftermarketEvent $ AftermarketBuyerEvent $ ChangeAftermarketBuyerScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    buyerSceneMenu :: AppNode
    buyerSceneMenu = hstack 
      [ spacer
      , buyerSceneButton "Own Bids" OwnAftermarketBids
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , buyerSceneButton "Bid History" AftermarketBidTransactions
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , buyerSceneButton "Aftermarket" PolicyAftermarketSales
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

      | old ^. #aftermarketModel % #cachedSales
      /= new ^. #aftermarketModel % #cachedSales = True

      | oldMarket ^. #buyerModel % #txFilterModel 
      /= newMarket ^. #buyerModel % #txFilterModel =
          bidHistoryRequiresUpdate
              (oldMarket ^. #buyerModel % #txFilterModel)
              (newMarket ^. #buyerModel % #txFilterModel)

      | otherwise = oldMarket /= newMarket

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
bidHistoryRequiresUpdate :: BidTxFilterModel -> BidTxFilterModel -> Bool
bidHistoryRequiresUpdate old new
  | old ^. #policyId /= new ^. #policyId = False
  | old ^. #dateRange /= new ^. #dateRange = False
  | otherwise = old /= new
