module P2PWallet.GUI.EventHandler.LendingEvent.ResearchEvent
  ( 
    handleResearchEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.SyncLoans
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Prelude

handleResearchEvent :: AppModel -> LoanResearchEvent -> [AppEventResponse AppModel AppEvent]
handleResearchEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeLoanResearchScene newScene -> 
    [ Model $ model & #lendingModel % #researchModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Set the new desired offer configuration.
  -----------------------------------------------
  InitializeLoanOfferConfiguration modal -> case modal of
    StartAdding _ ->
      let newOfferCfg = maybe def (toNewLoanOfferConfiguration reverseTickerMap) $ 
            lendingModel ^. #researchModel % #selectedLoanOfferConfiguration
       in [ Model $ model
              & #lendingModel % #researchModel % #newLoanOfferConfiguration ?~ newOfferCfg
          ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #researchModel % #newLoanOfferConfiguration .~ Nothing
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . InitializeLoanOfferConfiguration . AddResult) $ do
          -- `InitializeLoanOfferConfiguration StartAdding` is not always called so this can still
          -- be `Nothing`.
          let newOfferCfg = fromMaybe def $ lendingModel ^. #researchModel % #newLoanOfferConfiguration

          fromRightOrAppError $ verifyNewLoanOfferConfiguration tickerMap newOfferCfg
      ]
    AddResult verifiedOfferConfig ->
      [ Model $ model 
          -- Save the user supplied newLoanOfferConfiguration for quick editing.
          & #lendingModel % #researchModel % #offersFilterModel % #newLoanOfferConfiguration .~
              fromMaybe def (lendingModel ^. #researchModel % #newLoanOfferConfiguration)
          & #lendingModel % #researchModel % #newLoanOfferConfiguration .~ Nothing
          & #lendingModel % #researchModel % #selectedLoanOfferConfiguration ?~ verifiedOfferConfig
      , Task $ do
          -- Only sync the loan offers for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case lookupCachedOffers verifiedOfferConfig $ lendingModel ^. #researchModel % #cachedLoanOffers of
            Nothing -> return $ LendingEvent $ LoanResearchEvent $ SyncLoanOffers $ StartProcess Nothing
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Update the desired offer configuration.
  -----------------------------------------------
  UpdateLoanOfferConfiguration modal -> case modal of
    StartProcess _ ->
      [ Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . UpdateLoanOfferConfiguration . ProcessResults) $ do
          let newOfferCfg = lendingModel ^. #researchModel % #offersFilterModel % #newLoanOfferConfiguration

          fromRightOrAppError $ verifyNewLoanOfferConfiguration tickerMap newOfferCfg
      ]
    ProcessResults verifiedOfferConfig ->
      [ Model $ model 
          & #lendingModel % #researchModel % #selectedLoanOfferConfiguration ?~ verifiedOfferConfig
          & #forceRedraw %~ not
      , Task $ do
          -- Only sync the loan offers for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case lookupCachedOffers verifiedOfferConfig $ lendingModel ^. #researchModel % #cachedLoanOffers of
            Nothing -> return $ LendingEvent $ LoanResearchEvent $ SyncLoanOffers $ StartProcess Nothing
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Sync Loan Offers
  -----------------------------------------------
  SyncLoanOffers modal -> case modal of
    StartProcess _ ->
      [ Model $ model & #waitingStatus % #syncingLoanOffers .~ True
      , Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . SyncLoanOffers . ProcessResults) $ do
          offerConfig <- fromJustOrAppError "selectedLoanOfferConfiguration is Nothing" $ 
            lendingModel ^. #researchModel % #selectedLoanOfferConfiguration

          syncLoanOffers (config ^. #network) offerConfig $ 
            lendingModel ^. #researchModel % #cachedLoanOffers
      ]
    ProcessResults newCachedLoanOffers ->
      [ Model $ model 
          & #waitingStatus % #syncingLoanOffers .~ False
          & #lendingModel % #researchModel % #cachedLoanOffers .~ newCachedLoanOffers
      ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetResearchOffersFilters -> 
    [ Model $ model 
        & #lendingModel % #researchModel % #offersFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckResearchOffersFilterModel ->
    let newCfg = lendingModel ^. #researchModel % #offersFilterModel % #newLoanOfferConfiguration in
      case verifyNewLoanOfferConfiguration tickerMap newCfg of
        Right _ -> [Event AppInit]
        Left err ->
          [ Model $ model
              & #lendingModel % #researchModel % #showOffersFilter .~ True -- keep it open
          , Event $ Alert err
          ]

  -----------------------------------------------
  -- Inspecting Borrower Info
  -----------------------------------------------
  InspectResearchBorrowerInformation borrower@(borrowerId,_) -> 
    [ Model $ model & #lendingModel % #researchModel % #inspectedBorrower ?~ borrower 
    , Event $ case Map.lookup borrowerId (lendingModel ^. #cachedBorrowerInfo) of
        Nothing -> LendingEvent $ LookupBorrowerInformation $ StartProcess $ Just borrower
        Just _ -> AppInit
    ]
  CloseInspectedResearchBorrowerInformation -> 
    [ Model $ model & #lendingModel % #researchModel % #inspectedBorrower .~ Nothing ]

  -----------------------------------------------
  -- Inspecting Target Loan Histories
  -----------------------------------------------
  InspectResearchLoanHistory loanId -> 
    [ Model $ model & #lendingModel % #researchModel % #inspectedLoan ?~ loanId 
    , Event $ case Map.lookup loanId (lendingModel ^. #cachedLoanHistories) of
        Nothing -> LendingEvent $ LookupLoanHistory $ StartProcess $ Just loanId
        Just _ -> AppInit
    ]
  CloseInspectedResearchLoanHistory -> 
    [ Model $ model & #lendingModel % #researchModel % #inspectedLoan .~ Nothing ]

