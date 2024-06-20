{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.TxBuilderEvent
  ( 
    handleTxBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BuildTxBody
import P2PWallet.Actions.SignTxBody
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
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
  -- Open the Change Popup
  -----------------------------------------------
  ShowTxChangePopup -> 
    [ Model $ model & #txBuilderModel % #showChangePopup .~ True ]

  -----------------------------------------------
  -- Remove User Input from Builder
  -----------------------------------------------
  RemoveSelectedUserInput idx ->
    [ Model $ model & #txBuilderModel % #userInputs %~ removeAction idx
                    & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Output from Builder
  -----------------------------------------------
  RemoveSelectedUserOutput idx ->
    [ Model $ model & #txBuilderModel % #userOutputs %~ removeAction idx
                    & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that User Output
  -----------------------------------------------
  ChangeUserOutputCount idx newCount ->
    [ Model $ model & #txBuilderModel % #userOutputs % ix idx % _2 % #count .~ newCount
                    & #txBuilderModel %~ balanceTx
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
                & #txBuilderModel %~ balanceTx
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
                & #txBuilderModel %~ balanceTx
      ]

  -----------------------------------------------
  -- Building transactions
  -----------------------------------------------
  BuildTx ->
    -- Build the transaction using the current `txBuilderModel`. It will calculate the fee
    -- and will return an updated `txBuilderModel`. The resulting tx.body file will be
    -- located in the tmp directory. If an error is throw, it will be displayed in an alert
    -- message. Otherwise, `BuildResult` will be called. This also ensures that the minUTxOValues
    -- are satisfied.
    [ Model $ model & #building .~ True 
    , Task $ runActionOrAlert (TxBuilderEvent . BuildResult) $
        buildTxBody (config ^. #network) txBuilderModel
    ]
  BuildResult newTx -> 
    -- Replace the old `txBuilderModel` with the new one that has the proper fee and disable
    -- the `building` flag. An `alertMessage` is used to tell the user the estimated transaction
    -- fee. Finally, now that is built, the `isBuilt` flag can be set to True to allow acting
    -- on the tx.body file currently in the tmp directory.
    [ Model $ 
        model & #txBuilderModel .~ newTx 
              & #building .~ False
              & #alertMessage .~ Just 
                  (fromString $ printf "Estimated Fee: %D ADA" (toAda $ newTx ^. #fee))
    ]

  -----------------------------------------------
  -- Sign Transaction
  -----------------------------------------------
  SignAndSubmitTx ->
    if not $ txBuilderModel ^. #isBuilt
    then [ Task $ return $ Alert "You must first build the transaction."]
    else
      [ Model $ model & #waitingOnDevice .~ True
      , Task $
          runActionOrAlert SubmitTx $ 
            signTxBody (config ^. #network) (model ^. #txBuilderModel)
      ]

