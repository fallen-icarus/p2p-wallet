{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.TxBuilderEvent
  ( 
    handleTxBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

handleTxBuilderEvent :: AppModel -> TxBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleTxBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeBuilderScene newScene -> 
    [ Model $ model & #txBuilderModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Reset the Builder
  -----------------------------------------------
  ResetBuilder -> 
    [ Model $ model & #txBuilderModel .~ def ]

  -----------------------------------------------
  -- Open the Add Popup
  -----------------------------------------------
  ShowTxAddPopup -> 
    [ Model $ model & #txBuilderModel % #showAddPopup .~ True ]

  -----------------------------------------------
  -- Remove User Input from Builder
  -----------------------------------------------
  RemoveSelectedUserInput idx ->
    [ Model $ model & #txBuilderModel % #userInputs %~ removeAction idx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Output from Builder
  -----------------------------------------------
  RemoveSelectedUserOutput idx ->
    [ Model $ model & #txBuilderModel % #userOutputs %~ removeAction idx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that User Output
  -----------------------------------------------
  ChangeUserOutputCount idx newCount ->
    [ Model $ model & #txBuilderModel % #userOutputs % ix idx % _2 % #count .~ newCount
    ]

  -----------------------------------------------
  -- Edit the User Output
  -----------------------------------------------
  EditSelectedUserOutput modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #targetUserOutput .~ 
          (mTarget & _Just % _2 %~ toNewUserOutput reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #targetUserOutput .~ Nothing ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . EditSelectedUserOutput . AddResult) $ do
          let (idx,newOutput) = fromMaybe (0,def) $ txBuilderModel ^. #targetUserOutput
          fromRightOrAppError $ fmap (idx,) $
            processNewUserOutput (config ^. #network) tickerMap fingerprintMap newOutput
      ]
    AddResult newInfo@(idx,_) ->
      [ Model $
          model & #txBuilderModel % #userOutputs % ix idx .~ newInfo
                & #txBuilderModel % #targetUserOutput .~ Nothing
      ]

  -----------------------------------------------
  -- Add the new change output
  -----------------------------------------------
  AddNewChangeOutput modal -> case modal of
    StartAdding _ ->
      [ Model $ model & #txBuilderModel % #newChangeOutput .~ def
                      & #txBuilderModel % #addingChangeOutput .~ True
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #newChangeOutput .~ def 
                      & #txBuilderModel % #addingChangeOutput .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . AddNewChangeOutput . AddResult) $ do
          fromRightOrAppError $
            processNewChangeOutput (config ^. #network) (txBuilderModel ^. #newChangeOutput)
      ]
    AddResult verifiedChangeOutput ->
      [ Model $
          model & #txBuilderModel % #changeOutput .~ Just verifiedChangeOutput
                & #txBuilderModel % #newChangeOutput .~ def
                & #txBuilderModel % #addingChangeOutput .~ False
      ]
