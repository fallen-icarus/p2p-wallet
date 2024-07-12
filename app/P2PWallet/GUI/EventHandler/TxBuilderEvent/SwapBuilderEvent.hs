module P2PWallet.GUI.EventHandler.TxBuilderEvent.SwapBuilderEvent
  ( 
    handleSwapBuilderEvent
  ) where

import Monomer

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
            processNewSwapCreation paymentAddress reverseTickerMap newSwapCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^. #parameters) 
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
