module P2PWallet.GUI.EventHandler.TxBuilderEvent
  ( 
    handleTxBuilderEvent
  ) where

import Monomer
import System.Directory qualified as Dir

import P2PWallet.Actions.AssembleWitnesses
import P2PWallet.Actions.BuildTxBody
import P2PWallet.Actions.ExportTxBody
import P2PWallet.Actions.WitnessTxBody
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
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
  -- Remove User Certificate from Builder
  -----------------------------------------------
  RemoveSelectedUserCertificate idx ->
    [ Model $ model & #txBuilderModel % #userCertificates %~ removeAction idx
                    & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove User Withdrawal from Builder
  -----------------------------------------------
  RemoveSelectedUserWithdrawal idx ->
    [ Model $ model & #txBuilderModel % #userWithdrawals %~ removeAction idx
                    & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Test Mint from Builder
  -----------------------------------------------
  RemoveTestMint ->
    [ Model $ model & #txBuilderModel % #testMint .~ Nothing
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
          fromRightOrAppError $ (idx,) <$>
            processNewUserOutput (config ^. #network) tickerMap fingerprintMap newOutput
      ]
    AddResult newInfo@(idx,_) ->
      [ Model $ model 
          & #txBuilderModel % #userOutputs % ix idx .~ newInfo
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
          model & #txBuilderModel % #changeOutput ?~ verifiedChangeOutput
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
      [ Model $ model & #txBuilderModel % #newTestMint .~ currentMint
                      & #txBuilderModel % #addingTestMint .~ True
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #newTestMint .~ def 
                      & #txBuilderModel % #addingTestMint .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . AddNewTestMint . AddResult) $ do
          fromRightOrAppError $
            processNewTestMint $ txBuilderModel ^. #newTestMint
      ]
    AddResult verifiedTestMint ->
      [ Model $
          model & #txBuilderModel % #testMint ?~ verifiedTestMint
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
      [ Model $ 
          model & #txBuilderModel .~ newTx 
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
      [ Model $ 
          -- The waitingOnDevice flag is left active. It will be disabled by whatever gets called
          -- next.
          model & #txBuilderModel % #keyWitnessFiles .~ witnessFiles
      , Task $ 
          if txBuilderModel ^. #allKeyWitnessesKnown then
            -- The witnesses can be assembled and then submitted to the blockchain.
            return $ TxBuilderEvent $ AssembleWitnesses StartProcess
          else
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
      [ Model $ 
          model & #waitingStatus % #waitingOnDevice .~ False
      , Task $ return $ SubmitTx signedFile
      ]

  -----------------------------------------------
  -- Exporting the tx.body file and witnesses
  -----------------------------------------------
  ExportTxBody modal -> case modal of
    StartProcess ->
      [ Task $ runActionOrAlert (TxBuilderEvent . ExportTxBody . ProcessResults) $
          exportTxBody (txBuilderModel ^. #keyWitnessFiles)
      ]
    ProcessResults exportDestination -> 
      [ Model $ 
          model & #waitingStatus % #waitingOnDevice .~ False -- This can be called from `WitnessTx`.
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
      [ Model $ model & #txBuilderModel % #importedSignedTxFile .~ ""
                      & #txBuilderModel % #importing .~ True
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #importedSignedTxFile .~ ""
                      & #txBuilderModel % #importing .~ False
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TxBuilderEvent . ImportSignedTxFile . AddResult) $ do
          let importedFile = toString $ txBuilderModel ^. #importedSignedTxFile

          -- Verify the file exists and is not a directory.
          unlessM (Dir.doesFileExist importedFile) $ throwIO $ AppError "File does not exist."

          -- Return the filepath.
          return importedFile
      ]
    AddResult importedFile ->
      [ Model $ model & #txBuilderModel % #importedSignedTxFile .~ ""
      , Task $ return $ SubmitTx $ SignedTxFile importedFile
      ]
