module P2PWallet.GUI.EventHandler.TxBuilderEvent.LoanBuilderEvent
  ( 
    handleLoanBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Prelude

handleLoanBuilderEvent :: AppModel -> LoanBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleLoanBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Remove Ask Creation from Builder
  -----------------------------------------------
  RemoveSelectedAskCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that Ask creation
  -----------------------------------------------
  ChangeAskCreationCount idx newCount ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCreations % ix idx % _2 % #count .~ newCount
        & #txBuilderModel %~ balanceTx
    ]

  -----------------------------------------------
  -- Edit the Ask Creation
  -----------------------------------------------
  EditSelectedAskCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ 
          (mTarget & _Just % _2 %~ toNewAskCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedAskCreation . AddResult) $ do
          (idx,newAskCreation) <- fromJustOrAppError "targetAskCreation is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetAskCreation
          verifiedAskCreation <- fromRightOrAppError $
            verifyNewAskCreation tickerMap newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedAskCreation & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedAskCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #askCreations % ix idx .~ newInfo
          & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new ask requires a deposit of: " <> display (verifiedAskCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Ask Close from Builder
  -----------------------------------------------
  RemoveSelectedAskClose idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Ask Update from Builder
  -----------------------------------------------
  RemoveSelectedAskUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Ask Update
  -----------------------------------------------
  EditSelectedAskUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ 
          fmap (fmap (toNewAskCreation reverseTickerMap . view #newAsk)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedAskUpdate . AddResult) $ do
          (idx,newAskCreation) <- fromJustOrAppError "targetAskUpdate is Nothing" $
            txBuilderModel ^. #loanBuilderModel % #targetAskUpdate

          verifiedAskCreation <- fromRightOrAppError $ verifyNewAskCreation tickerMap newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedAskCreation & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedAsk) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #askUpdates % ix idx % _2 % #newAsk .~ 
              verifiedAsk
          & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new ask UTxO requires a deposit of: " <> display (verifiedAsk ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Offer Creation from Builder
  -----------------------------------------------
  RemoveSelectedOfferCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Offer Creation
  -----------------------------------------------
  EditSelectedOfferCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ 
          (mTarget & _Just % _2 %~ toNewOfferCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedOfferCreation . AddResult) $ do
          (idx,newOfferCreation) <- fromJustOrAppError "targetOfferCreation is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetOfferCreation

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOfferCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferCreation` with the updated deposit field.
          return (idx, verifiedOfferCreation & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedOfferCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #offerCreations % ix idx .~ newInfo
          & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new offer requires a deposit of: " <> display (verifiedOfferCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Offer Close from Builder
  -----------------------------------------------
  RemoveSelectedOfferClose idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Offer Update from Builder
  -----------------------------------------------
  RemoveSelectedOfferUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Offer Update
  -----------------------------------------------
  EditSelectedOfferUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ 
          fmap (fmap (toNewOfferCreation reverseTickerMap . view #newOffer)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedOfferUpdate . AddResult) $ do
          (idx,newOfferCreation) <- fromJustOrAppError "targetOfferUpdate is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetOfferUpdate

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOfferCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferCreation` with the updated deposit field.
          return (idx, verifiedOfferCreation & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedOffer) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #offerUpdates % ix idx % _2 % #newOffer .~ 
              verifiedOffer
          & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new offer UTxO requires a deposit of: " <> display (verifiedOffer ^. #deposit)
          ]
      ]
