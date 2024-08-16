module P2PWallet.GUI.EventHandler.LendingEvent.BorrowEvent
  ( 
    handleBorrowEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleBorrowEvent :: AppModel -> BorrowEvent -> [AppEventResponse AppModel AppEvent]
handleBorrowEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeBorrowScene newScene -> 
    [ Model $ model & #lendingModel % #borrowModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Add new ask creation.
  -----------------------------------------------
  CreateNewAsk modal -> case modal of
    StartAdding _ -> 
      let LoanWallet{stakeCredential,loanAddress,alias,stakeKeyDerivation} = 
            lendingModel ^. #selectedWallet
          newAsk = 
            createNewAskCreation 
              (config ^. #network) 
              stakeCredential 
              stakeKeyDerivation 
              loanAddress 
              alias
       in [ Model $ model 
              & #lendingModel % #borrowModel % #newAskCreation ?~ newAsk
          ]
    CancelAdding -> 
      [ Model $ model 
          & #lendingModel % #borrowModel % #newAskCreation .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . BorrowEvent . CreateNewAsk . AddResult) $ do
          when (hasOfferActions $ txBuilderModel ^. #loanBuilderModel) $
            throwIO borrowAndLendError

          verifiedAskCreation <- fromRightOrAppError $
            verifyNewAskCreation tickerMap $ fromMaybe def $ 
              lendingModel ^. #borrowModel % #newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return $ verifiedAskCreation & #deposit .~ minUTxOValue
      ]
    AddResult verifiedAskCreation ->
      -- Get the index for the new ask creation.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #askCreations 
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #askCreations %~ 
                  flip snoc (newIdx,verifiedAskCreation)
              & #lendingModel % #borrowModel % #newAskCreation .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This ask requires a deposit of: " <> display (verifiedAskCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Add Ask Close to Builder
  -----------------------------------------------
  AddSelectedAskClose loanUTxO ->
    let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
        newInput = loanUTxOToAskClose network alias stakeCredential stakeKeyDerivation loanUTxO
    in  case processNewAskClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOfferActions $ txBuilderModel ^. #loanBuilderModel then
              [ Task $ runActionOrAlert (const AppInit) $ do
                  throwIO borrowAndLendError
              ]
            else
              [ Model $ model & #txBuilderModel .~ newTxModel
              , Task $ return $ Alert "Successfully added to builder!"
              ]

  -----------------------------------------------
  -- Add Selected Ask Update to Builder
  -----------------------------------------------
  AddSelectedAskUpdate modal -> case modal of
    StartAdding mTarget ->
      let LoanWallet{alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
          toNewCreation = loanUTxOToNewAskCreation 
            (config ^. #network) 
            alias 
            stakeCredential 
            stakeKeyDerivation 
            reverseTickerMap 
          newAskUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in [ Model $ model
              & #lendingModel % #borrowModel % #newAskUpdate .~ newAskUpdate
          ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #borrowModel % #newAskUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . BorrowEvent . AddSelectedAskUpdate . AddResult) $ do
          when (hasOfferActions $ txBuilderModel ^. #loanBuilderModel) $ 
            throwIO borrowAndLendError

          let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                lendingModel ^. #selectedWallet
          (utxoToClose@LoanUTxO{utxoRef},newAsk) <- 
            fromJustOrAppError "Nothing set for `newAskUpdate`" $ 
              lendingModel ^. #borrowModel % #newAskUpdate

          verifiedAskCreation <- fromRightOrAppError $ verifyNewAskCreation tickerMap newAsk

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This ask UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #askCloses
              , map (view $ _2 % #oldAsk % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #askUpdates
              ])

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskUpdate` with the updated deposit field.
          return $ AskUpdate
            { oldAsk = loanUTxOToAskClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newAsk = verifiedAskCreation & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedAskUpdate ->
      -- Get the index for the new ask update.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #askUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #askUpdates %~ 
                  flip snoc (newIdx,verifiedAskUpdate)
              & #lendingModel % #borrowModel % #newAskUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This ask requires a deposit of: " <> display (verifiedAskUpdate ^. #newAsk % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckOpenAsksFilterModel ->
    case checkOpenAsksFilterModel tickerMap $ lendingModel ^. #borrowModel % #openAsksFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #lendingModel % #borrowModel % #showOpenAsksFilter .~ True -- keep it open
        , Task $ runActionOrAlert (const AppInit) $ throwIO $ AppError err
        ]

  CheckLenderOffersFilterModel ->
    case checkLenderOffersFilterModel tickerMap $ lendingModel ^. #borrowModel % #lenderOffersFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #lendingModel % #borrowModel % #showLenderOffersFilter .~ True -- keep it open
        , Task $ runActionOrAlert (const AppInit) $ throwIO $ AppError err
        ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetOpenAsksFilters -> 
    [ Model $ model 
        & #lendingModel % #borrowModel % #openAsksFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetLenderOffersFilters -> 
    [ Model $ model 
        & #lendingModel % #borrowModel % #lenderOffersFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new ask close and add it to the builder. Balance the transaction after.
processNewAskClose :: AskClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewAskClose u@AskClose{utxoRef} model@TxBuilderModel{loanBuilderModel=LoanBuilderModel{..}} = do
  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This ask UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) askCloses
      , map (view $ _2 % #oldAsk % #utxoRef) askUpdates
      ])

  -- Get the input's new index.
  let newIdx = length askCloses

  -- Add the new close to the end of the list of ask closes.
  return $ balanceTx $ model & #loanBuilderModel % #askCloses %~ flip snoc (newIdx,u)
