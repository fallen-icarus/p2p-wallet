module P2PWallet.GUI.Widgets.Lending.Borrow 
  ( 
    borrowWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Borrow.ActiveLoans
import P2PWallet.GUI.Widgets.Lending.Borrow.CreditHistory
import P2PWallet.GUI.Widgets.Lending.Borrow.LenderOffers
import P2PWallet.GUI.Widgets.Lending.Borrow.OpenAsks
import P2PWallet.GUI.Widgets.Lending.Borrow.Transactions
import P2PWallet.Prelude

borrowWidget :: AppModel -> AppNode
borrowWidget model@AppModel{lendingModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH borrowSceneMenu
          , spacer
          , vscroll_ [wheelRate 50] $ vstack
              [ box_ [mergeRequired reqUpdate] (openAsksWidget model)
                  `nodeVisible` (lendingModel ^. #borrowModel % #scene == OpenAsks)
              , box_ [mergeRequired reqUpdate] (lenderOffersWidget model)
                  `nodeVisible` (lendingModel ^. #borrowModel % #scene == CurrentOffers)
              , box_ [mergeRequired reqUpdate] (activeLoansWidget model)
                  `nodeVisible` (lendingModel ^. #borrowModel % #scene == ActiveLoans)
              , box_ [mergeRequired reqUpdate] (creditHistoryWidget model)
                  `nodeVisible` (lendingModel ^. #borrowModel % #scene == CreditHistory)
              , box_ [mergeRequired reqUpdate] (transactionsWidget model)
                  `nodeVisible` (lendingModel ^. #borrowModel % #scene == BorrowerTransactions)
              ]
          ]
      , widgetIf (isJust $ lendingModel ^. #borrowModel % #inspectedBorrowerTransaction) $
          borrowerTxInspectionWidget model
        -- The inspected loan is here since only one loan can be inspected at a time.
        -- It doesn't make sense to move to other borrower scenes while inspecting a loan.
      , inspectLoanWidget model
          `nodeVisible` and
            [ isJust $ lendingModel ^. #borrowModel % #inspectedLoan
            -- Hide until after syncing is complete.
            , not $ model ^. #waitingStatus % #syncingLoanHistories
            ]
      ]
  where
    borrowSceneButton :: Text -> BorrowScene -> AppNode
    borrowSceneButton caption scene = do
      let dormantColor
            | lendingModel ^. #borrowModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | lendingModel ^. #borrowModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (LendingEvent $ BorrowEvent $ ChangeBorrowScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    borrowSceneMenu :: AppNode
    borrowSceneMenu = hstack 
      [ spacer
      , borrowSceneButton "Open Asks" OpenAsks
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , borrowSceneButton "Lender Offers" CurrentOffers
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , borrowSceneButton "Active Loans" ActiveLoans
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , borrowSceneButton "Credit History" CreditHistory
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , borrowSceneButton "Transactions" BorrowerTransactions
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{lendingModel=oldLending} new@AppModel{lendingModel=newLending} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | oldLending ^. #borrowModel % #openAsksFilterModel /=
        newLending ^. #borrowModel % #openAsksFilterModel =
          openAsksFilterModelRequiresUpdate 
              (oldLending ^. #borrowModel % #openAsksFilterModel)
              (newLending ^. #borrowModel % #openAsksFilterModel)
      | oldLending ^. #borrowModel % #lenderOffersFilterModel /=
        newLending ^. #borrowModel % #lenderOffersFilterModel =
          lenderOffersFilterModelRequiresUpdate 
              (oldLending ^. #borrowModel % #lenderOffersFilterModel)
              (newLending ^. #borrowModel % #lenderOffersFilterModel)
      | oldLending ^. #borrowModel % #txFilterModel /=
        newLending ^. #borrowModel % #txFilterModel =
          borrowerTxsRequiresUpdate
              (oldLending ^. #borrowModel % #txFilterModel)
              (newLending ^. #borrowModel % #txFilterModel)
      | otherwise = oldLending /= newLending

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
openAsksFilterModelRequiresUpdate :: OpenAsksFilterModel -> OpenAsksFilterModel -> Bool
openAsksFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #loanAsset /= new ^. #loanAsset = False
  | old ^. #minDuration /= new ^. #minDuration = False
  | old ^. #maxDuration /= new ^. #maxDuration = False
  | old ^. #collateral /= new ^. #collateral = False
  | otherwise = old /= new

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
lenderOffersFilterModelRequiresUpdate :: LenderOffersFilterModel -> LenderOffersFilterModel -> Bool
lenderOffersFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #loanAsset /= new ^. #loanAsset = False
  | old ^. #minDuration /= new ^. #minDuration = False
  | old ^. #maxDuration /= new ^. #maxDuration = False
  | old ^. #collateral /= new ^. #collateral = False
  | otherwise = old /= new

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
borrowerTxsRequiresUpdate :: BorrowTxFilterModel -> BorrowTxFilterModel -> Bool
borrowerTxsRequiresUpdate old new
  | old ^. #dateRange /= new ^. #dateRange = False
  | otherwise = old /= new
