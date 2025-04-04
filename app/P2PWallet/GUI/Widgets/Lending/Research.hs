module P2PWallet.GUI.Widgets.Lending.Research
  ( 
    researchWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests (inspectBorrowerWidget,inspectLoanWidget)
import P2PWallet.GUI.Widgets.Lending.Research.Actives
import P2PWallet.GUI.Widgets.Lending.Research.Offers
import P2PWallet.Prelude

researchWidget :: AppModel -> AppNode
researchWidget model@AppModel{lendingModel} = do
    zstack
      [ vstack 
          [ spacer
          , centerWidgetH researchSceneMenu
          , spacer
          , vscroll_ [wheelRate 50] $ vstack
              [ box_ [mergeRequired reqUpdate] (researchOffersWidget model)
                  `nodeVisible` (lendingModel ^. #researchModel % #scene == ResearchLoanOffers)
              , box_ [mergeRequired reqUpdate] (researchActivesWidget model)
                  `nodeVisible` (lendingModel ^. #researchModel % #scene == ResearchActiveLoans)
              ]
          ]
      -- The inspected borrower is here since only one borrower can be inspected at a time.
      -- It doesn't make sense to move to other scenes while inspecting a borrower.
      , widgetMaybe (lendingModel ^. #researchModel % #inspectedBorrower) $ \info ->
          let closeEvt = LendingEvent $ LoanResearchEvent CloseInspectedResearchBorrowerInformation 
              historyEvt = LendingEvent . LoanResearchEvent . InspectResearchLoanHistory
           in inspectBorrowerWidget info closeEvt historyEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingBorrowerInfo
                  ]
      -- The inspected loan is here since only one loan can be inspected at a time.
      -- It doesn't make sense to move to other scenes while inspecting a loan.
      , widgetMaybe (lendingModel ^. #researchModel % #inspectedLoan) $ \targetId ->
          let closeEvt = LendingEvent $ LoanResearchEvent CloseInspectedResearchLoanHistory in
          inspectLoanWidget targetId closeEvt model
            `nodeVisible` and
              [ -- Hide until after syncing is complete.
                not $ model ^. #waitingStatus % #syncingLoanHistories
              ]
      ]
  where
    researchSceneButton :: Text -> LoanResearchScene -> AppNode
    researchSceneButton caption scene = do
      let dormantColor
            | lendingModel ^. #researchModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | lendingModel ^. #researchModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (LendingEvent $ LoanResearchEvent $ ChangeLoanResearchScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    researchSceneMenu :: AppNode
    researchSceneMenu = hstack 
      [ spacer
      , researchSceneButton "Current Offers" ResearchLoanOffers
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , researchSceneButton "Active Loans" ResearchActiveLoans
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{lendingModel=oldLending} new@AppModel{lendingModel=newLending} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | oldLending ^. #researchModel % #offersFilterModel /=
        newLending ^. #researchModel % #offersFilterModel =
          offersFilterModelRequiresUpdate 
            (oldLending ^. #researchModel % #offersFilterModel)
            (newLending ^. #researchModel % #offersFilterModel)
      | oldLending ^. #researchModel % #activesFilterModel /=
        newLending ^. #researchModel % #activesFilterModel =
          activesFilterModelRequiresUpdate 
            (oldLending ^. #researchModel % #activesFilterModel)
            (newLending ^. #researchModel % #activesFilterModel)
      | otherwise = oldLending /= newLending

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
offersFilterModelRequiresUpdate :: OfferResearchFilterModel -> OfferResearchFilterModel -> Bool
offersFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #newLoanOfferConfiguration % #loanAsset /= new ^. #newLoanOfferConfiguration % #loanAsset = False
  | old ^. #newLoanOfferConfiguration % #minDuration /= new ^. #newLoanOfferConfiguration % #minDuration = False
  | old ^. #newLoanOfferConfiguration % #maxDuration /= new ^. #newLoanOfferConfiguration % #maxDuration = False
  | old ^. #newLoanOfferConfiguration % #collateral /= new ^. #newLoanOfferConfiguration % #collateral = False
  | otherwise = old /= new

-- Entering text fields can be laggy so updating the UI is delayed until the last possible second.
activesFilterModelRequiresUpdate :: ActiveResearchFilterModel -> ActiveResearchFilterModel -> Bool
activesFilterModelRequiresUpdate old new
  | old ^. #scene /= new ^. #scene = True
  | old ^. #sortingMethod /= new ^. #sortingMethod = True
  | old ^. #sortingDirection /= new ^. #sortingDirection = True
  | old ^. #newActiveLoanConfiguration % #loanAsset /= new ^. #newActiveLoanConfiguration % #loanAsset = False
  | old ^. #newActiveLoanConfiguration % #minDuration /= new ^. #newActiveLoanConfiguration % #minDuration = False
  | old ^. #newActiveLoanConfiguration % #maxDuration /= new ^. #newActiveLoanConfiguration % #maxDuration = False
  | old ^. #newActiveLoanConfiguration % #collateral /= new ^. #newActiveLoanConfiguration % #collateral = False
  | otherwise = old /= new
