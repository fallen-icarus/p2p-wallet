module P2PWallet.GUI.Widgets.Lending.Borrow 
  ( 
    borrowWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Borrow.LenderOffers
import P2PWallet.GUI.Widgets.Lending.Borrow.OpenAsks
import P2PWallet.Prelude

borrowWidget :: AppModel -> AppNode
borrowWidget model@AppModel{lendingModel} = do
    vstack 
      [ spacer
      , centerWidgetH borrowSceneMenu
      , spacer
      , box_ [mergeRequired reqUpdate] (openAsksWidget model)
          `nodeVisible` (lendingModel ^. #borrowModel % #scene == OpenAsks)
      , box_ [mergeRequired reqUpdate] (lenderOffersWidget model)
          `nodeVisible` (lendingModel ^. #borrowModel % #scene == CurrentOffers)
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
      , borrowSceneButton "Open Ask" OpenAsks
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
      | otherwise = True

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
  | otherwise = True

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
  | otherwise = True