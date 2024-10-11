module P2PWallet.GUI.Widgets.Lending.Lend
  ( 
    lendWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Lend.OfferHistory
import P2PWallet.GUI.Widgets.Lending.Lend.OpenOffers
import P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests
import P2PWallet.Prelude

lendWidget :: AppModel -> AppNode
lendWidget model@AppModel{lendingModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH lendSceneMenu
          , spacer
          , box_ [mergeRequired reqUpdate] (openOffersWidget model)
              `nodeVisible` (lendingModel ^. #lendModel % #scene == OpenOffers)
          , box_ [mergeRequired reqUpdate] (viewLoanRequestsWidget model)
              `nodeVisible` (lendingModel ^. #lendModel % #scene == ViewLoanRequests)
          , box_ [mergeRequired reqUpdate] (transactionsWidget model)
              `nodeVisible` (lendingModel ^. #lendModel % #scene == OfferHistory)
          ]
      , widgetIf (isJust $ lendingModel ^. #lendModel % #inspectedOfferTransaction) $
          offerTxInspectionWidget model
      -- The inspected borrower is here since only one borrower can be inspected at a time.
      -- It doesn't make sense to move to other lend scenes while inspecting a borrower.
      , widgetMaybe (lendingModel ^. #lendModel % #inspectedBorrower) $ \info ->
          let closeEvt = LendingEvent $ LendEvent CloseInspectedProspectiveBorrowerInformation
              historyEvt = LendingEvent . LendEvent . InspectTargetLoanHistory
           in inspectBorrowerWidget info closeEvt historyEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingBorrowerInfo
                  ]
      -- The inspected loan is here since only one loan can be inspected at a time.
      -- It doesn't make sense to move to other borrower scenes while inspecting a loan.
      , widgetMaybe (lendingModel ^. #lendModel % #inspectedLoan) $ \targetId ->
          let closeEvt = LendingEvent $ LendEvent CloseInspectedTargetLoanHistory in
          inspectLoanWidget targetId closeEvt model
            `nodeVisible` and
              [ -- Hide until after syncing is complete.
                not $ model ^. #waitingStatus % #syncingLoanHistories
              ]
      ]
  where
    lendSceneButton :: Text -> LendScene -> AppNode
    lendSceneButton caption scene = do
      let dormantColor
            | lendingModel ^. #lendModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | lendingModel ^. #lendModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (LendingEvent $ LendEvent $ ChangeLendScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    lendSceneMenu :: AppNode
    lendSceneMenu = hstack 
      [ spacer
      , lendSceneButton "Open Offers" OpenOffers
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , lendSceneButton "Offer History" OfferHistory
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , lendSceneButton "Browse Requests" ViewLoanRequests
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{lendingModel=oldLending} new@AppModel{lendingModel=newLending} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | oldLending ^. #lendModel % #openOffersFilterModel /=
        newLending ^. #lendModel % #openOffersFilterModel =
          openOffersFilterModelRequiresUpdate 
              (oldLending ^. #lendModel % #openOffersFilterModel)
              (newLending ^. #lendModel % #openOffersFilterModel)
      | oldLending ^. #lendModel % #txFilterModel /=
        newLending ^. #lendModel % #txFilterModel =
          offerHistoryRequiresUpdate
              (oldLending ^. #lendModel % #txFilterModel)
              (newLending ^. #lendModel % #txFilterModel)
      | otherwise = oldLending /= newLending

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
openOffersFilterModelRequiresUpdate :: OpenOffersFilterModel -> OpenOffersFilterModel -> Bool
openOffersFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #shouldBeExpired /= new ^. #shouldBeExpired = True
  | old ^. #loanAsset /= new ^. #loanAsset = False
  | old ^. #minDuration /= new ^. #minDuration = False
  | old ^. #maxDuration /= new ^. #maxDuration = False
  | old ^. #collateral /= new ^. #collateral = False
  | otherwise = old /= new

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
offerHistoryRequiresUpdate :: LendTxFilterModel -> LendTxFilterModel -> Bool
offerHistoryRequiresUpdate old new
  | old ^. #loanAsset /= new ^. #loanAsset = False
  | old ^. #collateral /= new ^. #collateral = False
  | old ^. #dateRange /= new ^. #dateRange = False
  | otherwise = old /= new
