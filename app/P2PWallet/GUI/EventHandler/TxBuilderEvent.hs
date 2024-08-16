module P2PWallet.GUI.EventHandler.TxBuilderEvent
  ( 
    handleTxBuilderEvent
  ) where

import Monomer
import System.Directory qualified as Dir

import P2PWallet.Actions.AssembleWitnesses
import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.BuildTxBody
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.ExportTxBody
import P2PWallet.Actions.WitnessTxBody
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.GUI.EventHandler.TxBuilderEvent.LoanBuilderEvent
import P2PWallet.GUI.EventHandler.TxBuilderEvent.SwapBuilderEvent
import P2PWallet.Plutus
import P2PWallet.Prelude

handleTxBuilderEvent :: AppModel -> TxBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleTxBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Reset the Builder
  -----------------------------------------------
  ResetBuilder -> 
    [ Model $ model & #txBuilderModel .~ def ]

  -----------------------------------------------
  -- Run swap event
  -----------------------------------------------
  SwapBuilderEvent swapEvent -> handleSwapBuilderEvent model swapEvent

  -----------------------------------------------
  -- Run loan event
  -----------------------------------------------
  LoanBuilderEvent loanEvent -> handleLoanBuilderEvent model loanEvent

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
  -- Open the Collateral Popup
  -----------------------------------------------
  ShowTxCollateralPopup -> 
    [ Model $ model & #txBuilderModel % #showCollateralPopup .~ True ]

  -----------------------------------------------
  -- Remove User Input from Builder
  -----------------------------------------------
  RemoveSelectedUserInput idx ->
    [ Model $ model 
        & #txBuilderModel % #userInputs %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Output from Builder
  -----------------------------------------------
  RemoveSelectedUserOutput idx ->
    [ Model $ model 
        & #txBuilderModel % #userOutputs %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Certificate from Builder
  -----------------------------------------------
  RemoveSelectedUserCertificate idx ->
    [ Model $ model 
        & #txBuilderModel % #userCertificates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Withdrawal from Builder
  -----------------------------------------------
  RemoveSelectedUserWithdrawal idx ->
    [ Model $ model 
        & #txBuilderModel % #userWithdrawals %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Test Mint from Builder
  -----------------------------------------------
  RemoveTestMint ->
    [ Model $ model 
        & #txBuilderModel % #testMint .~ Nothing
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that User Output
  -----------------------------------------------
  ChangeUserOutputCount idx newCount ->
    [ Model $ model 
        & #txBuilderModel % #userOutputs % ix idx % _2 % #count .~ newCount
        & #txBuilderModel %~ balanceTx
    ]

  -----------------------------------------------
  -- Edit the User Output
  -----------------------------------------------
  EditSelectedUserOutput modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model 
          & #txBuilderModel % #targetUserOutput .~ 
              (mTarget & _Just % _2 %~ toNewUserOutput reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #targetUserOutput .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (TxBuilderEvent . EditSelectedUserOutput . AddResult) $ do
          (idx,newOutput) <- fromJustOrAppError "targetUserOutput is Nothing" $ 
            txBuilderModel ^. #targetUserOutput

          verifiedOutput <- 
            fromRightOrAppError $ 
              verifyNewUserOutput (config ^. #network) tickerMap fingerprintMap newOutput

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                verifiedOutput

          when (minUTxOValue > verifiedOutput ^. #lovelace) $
            throwIO $ AppError $ minUTxOErrorMessage minUTxOValue

          return (idx,verifiedOutput)
      ]
    AddResult newInfo@(idx,_) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #userOutputs % ix idx .~ newInfo
          & #txBuilderModel % #targetUserOutput .~ Nothing
          & #txBuilderModel %~ balanceTx
      ]

  -----------------------------------------------
  -- Add the new external user output
  -----------------------------------------------
  AddNewExternalUserOutput modal -> case modal of
    StartAdding _ ->
      [ Model $ model 
          & #txBuilderModel % #newExternalUserOutput .~ def
          & #txBuilderModel % #addingExternalUserOutput .~ True
      ]
    CancelAdding ->
      [ Model $ model 
          & #txBuilderModel % #newExternalUserOutput .~ def 
          & #txBuilderModel % #addingExternalUserOutput .~ False
      ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (TxBuilderEvent . AddNewExternalUserOutput . AddResult) $ do
          verifiedOutput <- 
            fromRightOrAppError $ 
              verifyNewUserOutput 
                (config ^. #network) 
                tickerMap
                fingerprintMap
                (txBuilderModel ^. #newExternalUserOutput)

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                verifiedOutput

          when (minUTxOValue > verifiedOutput ^. #lovelace) $
            throwIO $ AppError $ minUTxOErrorMessage minUTxOValue

          return verifiedOutput
      ]
    AddResult verifiedOutput ->
      -- Get the index for the new output.
      let newIdx = length $ model ^. #txBuilderModel % #userOutputs in
        [ Model $ model 
            & #waitingStatus % #addingToBuilder .~ False
            & #txBuilderModel % #addingExternalUserOutput .~ False
            & #txBuilderModel % #newExternalUserOutput .~ def 
            & #txBuilderModel % #userOutputs %~ flip snoc (newIdx,verifiedOutput)
            & #txBuilderModel %~ balanceTx
        , Task $ return $ Alert "Successfully added to builder!"
        ]

  -----------------------------------------------
  -- Add the new change output
  -----------------------------------------------
  AddNewChangeOutput modal -> case modal of
    StartAdding _ ->
      [ Model $ model 
          & #txBuilderModel % #newChangeOutput .~ 
              -- Populate the fields with the currently set change output.
              maybe def toNewChangeOutput (txBuilderModel ^. #changeOutput)
          & #txBuilderModel % #addingChangeOutput .~ True
      ]
    CancelAdding ->
      [ Model $ model 
          & #txBuilderModel % #newChangeOutput .~ def 
          & #txBuilderModel % #addingChangeOutput .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . AddNewChangeOutput . AddResult) $ do
          fromRightOrAppError $
            verifyNewChangeOutput (config ^. #network) (txBuilderModel ^. #newChangeOutput)
      ]
    AddResult verifiedChangeOutput ->
      [ Model $ model 
          & #txBuilderModel % #changeOutput ?~ verifiedChangeOutput
          & #txBuilderModel % #newChangeOutput .~ def
          & #txBuilderModel % #addingChangeOutput .~ False
          & #txBuilderModel %~ balanceTx
      ]

  -----------------------------------------------
  -- Add the new test mint
  -----------------------------------------------
  AddNewTestMint modal -> case modal of
    StartAdding _ ->
      -- Set the `newTestMint` field to either the current info if set, or a fresh
      -- entry.
      let currentMint = maybe def toNewTestMint $ txBuilderModel ^. #testMint in
      [ Model $ model 
          & #txBuilderModel % #newTestMint .~ currentMint
          & #txBuilderModel % #addingTestMint .~ True
      ]
    CancelAdding ->
      [ Model $ model 
          & #txBuilderModel % #newTestMint .~ def 
          & #txBuilderModel % #addingTestMint .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . AddNewTestMint . AddResult) $ do
          fromRightOrAppError $
            verifyNewTestMint $ txBuilderModel ^. #newTestMint
      ]
    AddResult verifiedTestMint ->
      [ Model $ model 
          & #txBuilderModel % #testMint ?~ verifiedTestMint
          & #txBuilderModel % #newTestMint .~ def
          & #txBuilderModel % #addingTestMint .~ False
          & #txBuilderModel %~ balanceTx
      ]

  -----------------------------------------------
  -- Convert the user's token name to hexidecimal
  -----------------------------------------------
  ConvertExampleTestMintNameToHexidecimal ->
    [ Model $ model
        & #txBuilderModel % #newTestMint % #exampleOutput .~
            toHexidecimal (txBuilderModel ^. #newTestMint % #exampleInput)
    ]

  -----------------------------------------------
  -- Building transactions
  -----------------------------------------------
  BuildTx modal -> case modal of
    StartProcess ->
      -- Build the transaction using the current `txBuilderModel`. It will calculate the fee
      -- and will return an updated `txBuilderModel`. The resulting tx.body file will be
      -- located in the tmp directory. If an error is throw, it will be displayed in an alert
      -- message. Otherwise, `BuildResult` will be called. 
      [ Model $ model & #waitingStatus % #building .~ True 
      , Task $ runActionOrAlert (TxBuilderEvent . BuildTx . ProcessResults) $
          buildTxBody (config ^. #network) txBuilderModel
      ]
    ProcessResults newTx -> 
      -- Replace the old `txBuilderModel` with the new one that has the proper fee and disable the
      -- `building` flag. An `alertMessage` is used to tell the user the estimated transaction fee.
      -- Finally, now that is built, the `isBuilt` is set to True to allow acting on the tx.body
      -- file currently in the tmp directory; this was already toggled by `buildTxBody`.
      [ Model $ model 
          & #txBuilderModel .~ newTx 
          & #waitingStatus % #building .~ False
          & #alertMessage ?~
              fromString (printf "Estimated Fee: %D ADA" (toAda $ newTx ^. #fee))
      ]

  -----------------------------------------------
  -- Witnessing transactions
  -----------------------------------------------
  WitnessTx modal -> case modal of
    StartProcess ->
      if not $ txBuilderModel ^. #isBuilt
      then [ Task $ return $ Alert "The transaction must first be built." ]
      else
        [ Model $ model & #waitingStatus % #waitingOnDevice .~ True 
        , Task $ runActionOrAlert (TxBuilderEvent . WitnessTx . ProcessResults) $
            witnessTxBody (config ^. #network) txBuilderModel
        ]
    ProcessResults witnessFiles -> 
      [ Model $ model
          -- The waitingOnDevice flag is left active. It will be disabled by whatever gets called
          -- next.
          & #txBuilderModel % #keyWitnessFiles .~ witnessFiles
      , Task $ case txBuilderModel ^. #txType of
          PairedTx -> 
            -- The witnesses can be assembled and then submitted to the blockchain.
            return $ TxBuilderEvent $ AssembleWitnesses StartProcess
          _ ->
            -- Export the tx.body file so it can be signed externally. The witnesses will also be
            -- exported.
            return $ TxBuilderEvent $ ExportTxBody StartProcess
      ]

  -----------------------------------------------
  -- Assembling witnesses
  -----------------------------------------------
  AssembleWitnesses modal -> case modal of
    StartProcess ->
      [ Task $ runActionOrAlert (TxBuilderEvent . AssembleWitnesses . ProcessResults) $
          assembleWitnesses (txBuilderModel ^. #keyWitnessFiles)
      ]
    ProcessResults signedFile -> 
      [ Model $ model 
          & #waitingStatus % #waitingOnDevice .~ False
      , Task $ return $ SubmitTx signedFile
      ]

  -----------------------------------------------
  -- Exporting the tx.body file and witnesses
  -----------------------------------------------
  ExportTxBody modal -> case modal of
    StartProcess ->
      [ Task $ runActionOrAlert (TxBuilderEvent . ExportTxBody . ProcessResults) $
          exportTxBody (txBuilderModel ^. #targetPath) (txBuilderModel ^. #keyWitnessFiles)
      ]
    ProcessResults exportDestination -> 
      [ Model $ model 
          & #waitingStatus % #waitingOnDevice .~ False -- This can be called from `WitnessTx`.
      , Task $ return $ Alert $ unlines
          [ "Transaction file(s) successfully exported to the following directory:"
          , toText exportDestination
          ]
      ]

  -----------------------------------------------
  -- Import tx.signed
  -----------------------------------------------
  ImportSignedTxFile modal -> case modal of
    StartAdding _ ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ ""
          & #txBuilderModel % #importing .~ True
      ]
    CancelAdding ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ ""
          & #txBuilderModel % #importing .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . ImportSignedTxFile . AddResult) $ do
          let rawPath = toString $ txBuilderModel ^. #targetPath

          -- Expand the filepath.
          expandedPath <- expandFilePath rawPath >>= 
            fromJustOrAppError ("Not a valid file path: " <> toText rawPath)

          -- Verify the file exists and is not a directory.
          unlessM (Dir.doesFileExist expandedPath) $ 
            throwIO $ AppError $ "File does not exist: " <> toText expandedPath

          -- Return the filepath.
          return expandedPath
      ]
    AddResult importedFile ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ ""
          & #txBuilderModel % #importing .~ False
      , Task $ return $ SubmitTx $ SignedTxFile importedFile
      ]

  -----------------------------------------------
  -- Get export directory
  -----------------------------------------------
  GetTxFileExportDirectory modal -> case modal of
    StartAdding _ ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ ""
          & #txBuilderModel % #exporting .~ True
      ]
    CancelAdding ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ ""
          & #txBuilderModel % #exporting .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . GetTxFileExportDirectory . AddResult) $ do
          let rawPath = toString $ txBuilderModel ^. #targetPath

          -- Expand the path to get the absolute path.
          expandedPath <- 
            expandFilePath rawPath >>= fromJustOrAppError ("Not a valid path: " <> toText rawPath)

          -- Verify the destination is not a file.
          Dir.doesFileExist (toString expandedPath) >>= \exists -> when exists $
            throwIO $ AppError "Destination directory is a file."

          return expandedPath
      ]
    AddResult fullPath ->
      [ Model $ model 
          & #txBuilderModel % #targetPath .~ toText fullPath
          & #txBuilderModel % #exporting .~ False
      , Task $ return $ do
          if txBuilderModel ^. #txType == WatchedTx then
            TxBuilderEvent $ ExportTxBody StartProcess
          else
            TxBuilderEvent $ WitnessTx StartProcess
      ]
