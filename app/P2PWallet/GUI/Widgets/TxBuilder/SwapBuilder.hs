module P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder
  ( 
    swapsActionCount
  , swapCreationsList
  , editSwapCreationWidget
  , swapClosesList
  , swapUpdatesList
  , editSwapUpdateWidget
  , swapExecutionsList
  , editSwapExecutionWidget
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapCloses
import P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapCreations
import P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapExecutions
import P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapUpdates
import P2PWallet.Prelude

swapsActionCount :: SwapBuilderModel -> Int
swapsActionCount SwapBuilderModel{..} = sum
  [ length swapCreations
  , length swapCloses
  , length swapUpdates
  , length swapExecutions
  ]
