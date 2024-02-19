module P2PWallet.GUI.EventHandler.TxBuilderEvent
  ( 
    handleTxBuilderEvent
  ) where

import Monomer
import Data.Maybe (fromJust)

import P2PWallet.Actions.BuildTxBody
import P2PWallet.Actions.ExportTxBody
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Files
import P2PWallet.Data.Lens
import P2PWallet.Prelude

handleTxBuilderEvent
  :: AppModel
  -> TxBuilderEvent
  -> [AppEventResponse AppModel AppEvent]
handleTxBuilderEvent model@AppModel{_config=Config{_network}, _txBuilderModel} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeBuilderScene newScene -> 
    -- Whenever the builder scene is changed, reset the `new` fields to clear the widgets from
    -- the last usages.
    [ Model $ model & txBuilderModel . scene .~ newScene 
                    & txBuilderModel . newOutput .~ def
                    & txBuilderModel . newInput .~ def 
    ]

  -----------------------------------------------
  -- Building Transactions
  -----------------------------------------------
  ResetBuilder ->
    -- Reset all fields in the `txBuilderModel`.
    [ Model $ model & txBuilderModel .~ def ]
  InsertNewOutput ->
    -- Indices are used to keep track of which output is being edited. If the index is 0, then
    -- this is a new output. Otherwise, it is editing the output with the corresponding index.
    case processNewOutput _network _txBuilderModel of
      Left err -> 
        -- If there was an error processing the new output, display the error to the user. Don't
        -- change the scene to make it easy for the user to quickly retry.
        [ Model $ model & alertMessage .~ Just err ]
      Right newBalancedTx ->
        -- If processing was successfull, close the `addOutput` widget and return the user
        -- to the transaction summary page. Also reset the `newOutput` field so that it is
        -- cleared for next time. Finally, disable the `isBuilt` flag since the new changes
        -- invalidate the old build.
        [ Model $ flip (set txBuilderModel) model $
            newBalancedTx & isBuilt .~ False
                          & scene .~ BuilderSummary
                          & newOutput .~ def
        ]
  EditOutput index ->
    -- Edit the output with the specified index. Editing is done by setting `newOutput` to
    -- the specified index and opening the `addOutput` widget. Once editing is done,
    -- `InsertNewOutput` will be called.
    let target = fromJust $ find ((==index) . fst) $ _txBuilderModel ^. outputs
    in [ Model $ model & txBuilderModel . newOutput .~ fmap fromVerifiedOutput target
                       & txBuilderModel . scene .~ BuilderAddNewOutput 
       ]
  DeleteOutput index ->
    -- Delete the output with the specified index. Re-index the remaining outputs (preserving
    -- ordering). The new `txBuilderModel` will need to be rebalanced with the fee reset to 0. 
    -- The `isBuilt` flag must be reset to false since the change invalidates the previous build. 
    let newOutputs = reIndex $ filter ((/= index) . fst) $ _txBuilderModel ^. outputs
        newBalancedTx = 
          balanceTx $ 
            _txBuilderModel & outputs .~ newOutputs
                            & txFee .~ 0
    in [ Model $ flip (set txBuilderModel) model $
          newBalancedTx & isBuilt .~ False
       ]
  InsertNewInput ->
    -- Indices are used to keep track of which input is being edited. If the index is 0, then
    -- this is a new input. Otherwise, it is editing the input with the corresponding index.
    case processNewInput (model ^. wallets) _txBuilderModel of
      Left err -> 
        -- If there was an error processing the new input, display the error to the user. Don't
        -- change the scene to make it easy for the user to quickly retry.
        [ Model $ model & alertMessage .~ Just err ]
      Right newBalancedTx ->
        -- If processing was successfull, close the `addInput` widget and return the user
        -- to the transaction summary page. Also reset the `newInput` field so that it is
        -- cleared for next time. Finally, disable the `isBuilt` flag since the new changes
        -- invalidate the old build.
        [ Model $ flip (set txBuilderModel) model $
            newBalancedTx & isBuilt .~ False
                          & scene .~ BuilderSummary
                          & newInput .~ def
        ]
  EditInput index ->
    -- Edit the input with the specified index. Editing is done by setting `newInput` to
    -- the specified index and opening the `addInput` widget. Once editing is done,
    -- `InsertNewInput` will be called.
    let target = fromJust $ find ((==index) . fst) $ _txBuilderModel ^. inputs
    in [ Model $ model & txBuilderModel . newInput .~ fmap fromVerifiedInput target
                       & txBuilderModel . scene .~ BuilderAddNewInput
       ]
  DeleteInput index ->
    -- Delete the input with the specified index. Re-index the remaining inputs (perserving
    -- ordering). The new `txBuilderModel` will need to be rebalanced with the fee 
    -- reset to 0. The `isBuilt` flag must be reset to false since the change invalidates the 
    -- previous build. 
    let newInputs = reIndex $ filter ((/= index) . fst) $ _txBuilderModel ^. inputs
        newBalancedTx = 
          balanceTx $ 
            _txBuilderModel & inputs .~ newInputs
                            & txFee .~ 0
    in [ Model $ flip (set txBuilderModel) model $
          newBalancedTx & isBuilt .~ False
       ]
  InsertNewChangeOutput ->
    -- Check if the new change output is valid.
    case processNewChangeOutput _network _txBuilderModel of
      Left err -> 
        -- If there was an error processing the new change output, display the error to the user. 
        -- Don't change the scene to make it easy for the user to quickly retry.
        [ Model $ model & alertMessage .~ Just err ]
      Right newBalancedTx ->
        -- If processing was successfull, close the `addChangeOutput` widget and return the user
        -- to the transaction summary page. Also reset the `newChangeOutput` field so that it is
        -- cleared for next time. Finally, disable the `isBuilt` flag since the new changes
        -- invalidate the old build.
        [ Model $ flip (set txBuilderModel) model $
            newBalancedTx & isBuilt .~ False
                          & scene .~ BuilderSummary
                          & newChangeOutput .~ def
        ]
  EditChangeOutput ->
    -- Edit the current change output. `InsertNewChangeOutput` will be called after editing.
    [ Model $ model & txBuilderModel . scene .~ BuilderAddChangeOutput
                    & txBuilderModel . newChangeOutput .~ 
                        (fromVerifiedChangeOutput $ _txBuilderModel ^. changeOutput)
    ]
  InsertNewCertificate ->
    -- Check if the new certificate is valid.
    case processNewCertificate _network _txBuilderModel of
      Left err -> 
        -- If there was an error processing the new certificate, display the error to the user. 
        -- Don't change the scene to make it easy for the user to quickly retry.
        [ Model $ model & alertMessage .~ Just err ]
      Right newBalancedTx ->
        -- If processing was successfull, close the `addCertificate` widget and return the user
        -- to the transaction summary page. Also reset the `newCertificate` field so that it is
        -- cleared for next time. Finally, disable the `isBuilt` flag since the new changes
        -- invalidate the old build.
        [ Model $ flip (set txBuilderModel) model $
            newBalancedTx & isBuilt .~ False
                          & scene .~ BuilderSummary
                          & newCertificate .~ def
        ]
  EditCertificate index ->
    -- Edit the certificate with the specified index. Editing is done by setting `newCertificate` to
    -- the specified index and opening the `addCertificate` widget. Once editing is done,
    -- `InsertNewCertificate` will be called.
    let target = fromJust $ find ((==index) . fst) $ _txBuilderModel ^. certificates
    in [ Model $ model & txBuilderModel . newCertificate .~ fmap fromVerifiedCertificate target
                       & txBuilderModel . scene .~ BuilderAddNewCertificate 
       ]
  DeleteCertificate index ->
    -- Delete the certificate with the specified index. Re-index the remaining certificates 
    -- (preserving ordering). The new `txBuilderModel` will need to be rebalanced with the fee 
    -- reset to 0. The `isBuilt` flag must be reset to false since the change invalidates the 
    -- previous build. 
    let newCerts = reIndex $ filter ((/= index) . fst) $ _txBuilderModel ^. certificates
        newBalancedTx = 
          balanceTx $ 
            _txBuilderModel & certificates .~ newCerts
                            & txFee .~ 0
    in [ Model $ flip (set txBuilderModel) model $
          newBalancedTx & isBuilt .~ False
       ]
  InsertNewWithdrawal ->
    -- Check if the new withdrawal is valid.
    case processNewWithdrawal _network _txBuilderModel of
      Left err -> 
        -- If there was an error processing the new withdrawal, display the error to the user. 
        -- Don't change the scene to make it easy for the user to quickly retry.
        [ Model $ model & alertMessage .~ Just err ]
      Right newBalancedTx ->
        -- If processing was successfull, close the `addWithdrawal` widget and return the user
        -- to the transaction summary page. Also reset the `nwWithdrawal` field so that it is
        -- cleared for next time. Finally, disable the `isBuilt` flag since the new changes
        -- invalidate the old build.
        [ Model $ flip (set txBuilderModel) model $
            newBalancedTx & isBuilt .~ False
                          & scene .~ BuilderSummary
                          & newWithdrawal .~ def
        ]
  EditWithdrawal index ->
    -- Edit the withdrawal with the specified index. Editing is done by setting `newWithdrawal` to
    -- the specified index and opening the `addWithdrawal` widget. Once editing is done,
    -- `InsertNewWithdrawal` will be called.
    let target = fromJust $ find ((==index) . fst) $ _txBuilderModel ^. withdrawals
    in [ Model $ model & txBuilderModel . newWithdrawal .~ fmap fromVerifiedWithdrawal target
                       & txBuilderModel . scene .~ BuilderAddNewWithdrawal 
       ]
  DeleteWithdrawal index ->
    -- Delete the withdrawal with the specified index. Re-index the remaining withdrawals 
    -- (preserving ordering). The new `txBuilderModel` will need to be rebalanced with the fee 
    -- reset to 0. The `isBuilt` flag must be reset to false since the change invalidates the 
    -- previous build. 
    let newWtdrs = reIndex $ filter ((/= index) . fst) $ _txBuilderModel ^. withdrawals
        newBalancedTx = 
          balanceTx $ 
            _txBuilderModel & withdrawals .~ newWtdrs
                            & txFee .~ 0
    in [ Model $ flip (set txBuilderModel) model $
          newBalancedTx & isBuilt .~ False
       ]
  BuildTx ->
    -- Build the transaction using the current `txBuilderModel`. It will calculate the fee
    -- and will return an updated `txBuilderModel`. The resulting tx.body file will be
    -- located in the tmp directory. If an error is throw, it will be displayed in an alert
    -- message. Otherwise, `BuildResult` will be called.
    [ Model $ model & building .~ True 
    , Task $ 
        runActionOrAlert
          (TxBuilderEvent . BuildResult)
          (buildTxBody _network _txBuilderModel)
    ]
  BuildResult newTx -> 
    -- Replace the old `txBuilderModel` with the new one that has the proper fee and disable
    -- the `building` flag. An `alertMessage` is used to tell the user the estimated transaction
    -- fee. Finally, now that is built, the `isBuilt` flag can be set to True to allow acting
    -- on the tx.body file currently in the tmp directory.
    [ Model $ 
        model & txBuilderModel .~ newTx 
              & building .~ False
              & txBuilderModel . isBuilt .~ True
              & alertMessage .~ Just 
                  (fromString $ printf "Estimated Fee: %D ADA" (toADA $ newTx ^. txFee))
    ]

  -----------------------------------------------
  -- Exporting the transaction.
  -----------------------------------------------
  ExportTxBody ->
    -- Exporting the transaction can only be done if `isBuilt` is set to True since that means
    -- the tx.body file in the tmp directory is up-to-date. If there is an error, it will be shown
    -- in the `alertMessage`. If it is successfull, the success message will be shown in the 
    -- `alertMessage` alongside the absolute path to the exported file. It is currently hardcoded
    -- to be exported to the user's $HOME directory.
    [ Task $
        if not $ _txBuilderModel ^. isBuilt
        then return $ Alert "You must first build the transaction."
        else runActionOrAlert (TxBuilderEvent . ExportTxBodyResult) $ 
               exportTxBody $ TxBodyFile $ toString $ model ^. extraTextField
    ]
  ExportTxBodyResult successMsg ->
    -- Close the export widget, return to the summary page, and display a success message.
    -- Reset the extraTextField for next time.
    [ Model $ model & txBuilderModel . scene .~ BuilderSummary
                    & extraTextField .~ ""
    , Task $
        return $ Alert successMsg
    ]
