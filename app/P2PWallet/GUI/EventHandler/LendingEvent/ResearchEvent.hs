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
  ResetResearchActivesFilters -> 
    [ Model $ model 
        & #lendingModel % #researchModel % #activesFilterModel .~ def
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
  CheckResearchActiveFilterModel ->
    let newCfg = lendingModel ^. #researchModel % #activesFilterModel % #newActiveLoanConfiguration in
      case verifyNewActiveLoanConfiguration tickerMap newCfg of
        Right _ -> [Event AppInit]
        Left err ->
          [ Model $ model
              & #lendingModel % #researchModel % #showActivesFilter .~ True -- keep it open
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
        Nothing -> LendingEvent $ LookupLoanHistories $ StartProcess $ Just [loanId]
        Just _ -> AppInit
    ]
  CloseInspectedResearchLoanHistory -> 
    [ Model $ model & #lendingModel % #researchModel % #inspectedLoan .~ Nothing ]

  -----------------------------------------------
  -- Sync Active Loans
  -----------------------------------------------
  SyncActiveLoans modal -> case modal of
    StartProcess _ ->
      [ Model $ model & #waitingStatus % #syncingActiveLoans .~ True
      , Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . SyncActiveLoans . ProcessResults) $ do
          activeConfig <- fromJustOrAppError "selectedActiveLoanConfiguration is Nothing" $ 
            lendingModel ^. #researchModel % #selectedActiveLoanConfiguration

          syncActiveLoans (config ^. #network) activeConfig $ 
            lendingModel ^. #researchModel % #cachedActiveLoans
      ]
    ProcessResults newCachedActiveLoans ->
      [ Model $ model 
          & #waitingStatus % #syncingActiveLoans .~ False
          & #lendingModel % #researchModel % #cachedActiveLoans .~ newCachedActiveLoans
      ]

  -----------------------------------------------
  -- Set the new desired active loan configuration.
  -----------------------------------------------
  InitializeActiveLoanConfiguration modal -> case modal of
    StartAdding _ ->
      let newActiveCfg = maybe def (toNewActiveLoanConfiguration reverseTickerMap) $ 
            lendingModel ^. #researchModel % #selectedActiveLoanConfiguration
       in [ Model $ model
              & #lendingModel % #researchModel % #newActiveLoanConfiguration ?~ newActiveCfg
          ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #researchModel % #newActiveLoanConfiguration .~ Nothing
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . InitializeActiveLoanConfiguration . AddResult) $ do
          -- `InitializeActiveLoanConfiguration StartAdding` is not always called so this can still
          -- be `Nothing`.
          let newActiveCfg = fromMaybe def $ lendingModel ^. #researchModel % #newActiveLoanConfiguration

          fromRightOrAppError $ verifyNewActiveLoanConfiguration tickerMap newActiveCfg
      ]
    AddResult verifiedActiveConfig ->
      [ Model $ model 
          -- -- Save the user supplied newActiveLoanConfiguration for quick editing.
          & #lendingModel % #researchModel % #activesFilterModel % #newActiveLoanConfiguration .~
              fromMaybe def (lendingModel ^. #researchModel % #newActiveLoanConfiguration)
          & #lendingModel % #researchModel % #newActiveLoanConfiguration .~ Nothing
          & #lendingModel % #researchModel % #selectedActiveLoanConfiguration ?~ verifiedActiveConfig
      , Task $ do
          -- Only sync the active loans for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case lookupCachedActiveLoans verifiedActiveConfig $ lendingModel ^. #researchModel % #cachedActiveLoans of
            Nothing -> return $ LendingEvent $ LoanResearchEvent $ SyncActiveLoans $ StartProcess Nothing
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Update the desired active loan configuration.
  -----------------------------------------------
  UpdateActiveLoanConfiguration modal -> case modal of
    StartProcess _ ->
      [ Task $ runActionOrAlert (LendingEvent . LoanResearchEvent . UpdateActiveLoanConfiguration . ProcessResults) $ do
          let newActiveCfg = lendingModel ^. #researchModel % #activesFilterModel % #newActiveLoanConfiguration

          fromRightOrAppError $ verifyNewActiveLoanConfiguration tickerMap newActiveCfg
      ]
    ProcessResults verifiedActiveConfig ->
      [ Model $ model 
          & #lendingModel % #researchModel % #selectedActiveLoanConfiguration ?~ verifiedActiveConfig
          & #forceRedraw %~ not
      , Task $ do
          -- Only sync the loan offers for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case lookupCachedActiveLoans verifiedActiveConfig $ lendingModel ^. #researchModel % #cachedActiveLoans of
            Nothing -> return $ LendingEvent $ LoanResearchEvent $ SyncActiveLoans $ StartProcess Nothing
            Just _ -> return AppInit
      ]
