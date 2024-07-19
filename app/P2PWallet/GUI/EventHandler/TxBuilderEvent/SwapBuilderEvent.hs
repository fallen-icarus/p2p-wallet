module P2PWallet.GUI.EventHandler.TxBuilderEvent.SwapBuilderEvent
  ( 
    handleSwapBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Prelude

handleSwapBuilderEvent :: AppModel -> SwapBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleSwapBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Remove Swap Creation from Builder
  -----------------------------------------------
  RemoveSelectedSwapCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #swapBuilderModel % #swapCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Swap Close from Builder
  -----------------------------------------------
  RemoveSelectedSwapClose idx ->
    [ Model $ model 
        & #txBuilderModel % #swapBuilderModel % #swapCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Swap Update from Builder
  -----------------------------------------------
  RemoveSelectedSwapUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #swapBuilderModel % #swapUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Swap Execution from Builder
  -----------------------------------------------
  RemoveSelectedSwapExecution idx ->
    [ Model $ model 
        & #txBuilderModel % #swapBuilderModel % #swapExecutions %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that Swap creation
  -----------------------------------------------
  ChangeSwapCreationCount idx newCount ->
    [ Model $ model 
        & #txBuilderModel % #swapBuilderModel % #swapCreations % ix idx % _2 % #count .~ newCount
        & #txBuilderModel %~ balanceTx
    ]

  -----------------------------------------------
  -- Edit the Swap Creation
  -----------------------------------------------
  EditSelectedSwapCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapCreation .~ 
          (mTarget & _Just % _2 %~ toNewSwapCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (swapBuilderEvent . EditSelectedSwapCreation . AddResult) $ do
          let (idx,newSwapCreation@NewSwapCreation{paymentAddress}) = 
                fromMaybe (0,def) $ txBuilderModel ^. #swapBuilderModel % #targetSwapCreation
          verifiedSwap <- fromRightOrAppError $ 
            verifyNewSwapCreation paymentAddress reverseTickerMap newSwapCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                (emptySwapBuilderModel & #swapCreations .~ [(0,verifiedSwap)])

          -- Return the `SwapCreation` with the updated deposit field.
          return (idx, verifiedSwap & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedSwapCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #swapBuilderModel % #swapCreations % ix idx .~ newInfo
          & #txBuilderModel % #swapBuilderModel % #targetSwapCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new swap requires a deposit of: " <> display (verifiedSwapCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Edit the Swap Update
  -----------------------------------------------
  EditSelectedSwapUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapUpdate .~ 
          fmap (fmap (toNewSwapCreation reverseTickerMap . view #newSwap)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (swapBuilderEvent . EditSelectedSwapUpdate . AddResult) $ do
          let (idx,newSwapCreation@NewSwapCreation{paymentAddress}) = 
                fromMaybe (0,def) $ txBuilderModel ^. #swapBuilderModel % #targetSwapUpdate
          verifiedSwap <- fromRightOrAppError $ 
            verifyNewSwapCreation paymentAddress reverseTickerMap newSwapCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                (emptySwapBuilderModel & #swapCreations .~ [(0,verifiedSwap)])

          -- Return the `SwapCreation` with the updated deposit field.
          return (idx, verifiedSwap & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedSwap) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #swapBuilderModel % #swapUpdates % ix idx % _2 % #newSwap .~ 
              verifiedSwap
          & #txBuilderModel % #swapBuilderModel % #targetSwapUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new swap requires a deposit of: " <> display (verifiedSwap ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Edit the Swap Execution
  -----------------------------------------------
  EditSelectedSwapExecution modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapExecution .~ 
          fmap (fmap (toNewSwapExecution reverseTickerMap)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #swapBuilderModel % #targetSwapExecution .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (swapBuilderEvent . EditSelectedSwapExecution . AddResult) $ do
          (idx,newSwap) <- fromJustOrAppError "Nothing set for `targetSwapExecution`" $
            txBuilderModel ^. #swapBuilderModel % #targetSwapExecution

          verifiedSwapExecution <- fromRightOrAppError $ 
            verifyNewSwapExecution reverseTickerMap newSwap

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network)
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank swapBuilderModel to calculate the minUTxOValue for the new swap.
                (emptySwapBuilderModel & #swapExecutions .~ [(0,verifiedSwapExecution)])

          -- Check if the swap output contains enough ADA. Also account for whether ada is the 
          -- part of the trading pair.
          (idx,) <$> fromRightOrAppError (updateMinUTxO verifiedSwapExecution minUTxOValue)
      ]
    AddResult (idx,verifiedSwap@SwapExecution{lovelace,deposit}) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #swapBuilderModel % #swapExecutions % ix idx % _2 .~ 
              verifiedSwap
          & #txBuilderModel % #swapBuilderModel % #targetSwapExecution .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ do
          let successMsg
                | lovelace >= deposit = "Successfully added to builder!"
                | otherwise = unlines
                    [ "Successfully added to builder!"
                    , ""
                    , "The swap minUTxOValue increased by: " <> display (deposit - lovelace)
                    , "You will need to cover the increase to execute this swap."
                    ]
          return $ Alert successMsg
      ]
