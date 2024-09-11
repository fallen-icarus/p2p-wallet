module P2PWallet.GUI.EventHandler.TxBuilderEvent.OptionsBuilderEvent
  ( 
    handleOptionsBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

handleOptionsBuilderEvent :: AppModel -> OptionsBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleOptionsBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Remove Proposal Creation from Builder
  -----------------------------------------------
  RemoveSelectedProposalCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #proposalCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that proposal creation
  -----------------------------------------------
  ChangeProposalCreationCount idx newCount ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #proposalCreations % ix idx % _2 % #count .~ newCount
        & #txBuilderModel %~ balanceTx
    ]

  -----------------------------------------------
  -- Edit the Proposal Creation
  -----------------------------------------------
  EditSelectedProposalCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetProposalCreation .~ 
          (mTarget & _Just % _2 %~ toNewProposalCreation reverseTickerMap (config ^. #timeZone))
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetProposalCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (optionsBuilderEvent . EditSelectedProposalCreation . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetProposalCreation is Nothing" $ 
            txBuilderModel ^. #optionsBuilderModel % #targetProposalCreation

          verifiedProposal <- fromRightOrAppError $
            verifyNewProposalCreation 
              reverseTickerMap 
              tickerMap 
              (config ^. #timeZone) 
              (config ^. #currentTime) 
              newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                  -- proposal.
                  (emptyOptionsBuilderModel & #proposalCreations .~ [(0,verifiedProposal)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                -- proposal.
                (emptyOptionsBuilderModel & #proposalCreations .~ 
                  [(0,verifiedProposal & #deposit .~ minUTxOValue1)])

          -- Return the `ProposalCreation` with the updated deposit field.
          return (idx, verifiedProposal & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedProposal) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #optionsBuilderModel % #proposalCreations % ix idx .~ newInfo
          & #txBuilderModel % #optionsBuilderModel % #targetProposalCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new options proposal requires a deposit of: " <> display (verifiedProposal ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Proposal Close from Builder
  -----------------------------------------------
  RemoveSelectedProposalClose idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #proposalCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Proposal Update from Builder
  -----------------------------------------------
  RemoveSelectedProposalUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #proposalUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Proposal Update
  -----------------------------------------------
  EditSelectedProposalUpdate modal -> case modal of
    StartAdding mTarget ->
      let timeZone = config ^. #timeZone in
        [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetProposalUpdate .~ 
            fmap (fmap (toNewProposalCreation reverseTickerMap timeZone . view #newProposal)) mTarget
        ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetProposalUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (optionsBuilderEvent . EditSelectedProposalUpdate . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetProposalUpdate is Nothing" $
            txBuilderModel ^. #optionsBuilderModel % #targetProposalUpdate

          verifiedProposal <- fromRightOrAppError $
            verifyNewProposalCreation 
              reverseTickerMap 
              tickerMap 
              (config ^. #timeZone) 
              (config ^. #currentTime) 
              newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                  -- proposal.
                  (emptyOptionsBuilderModel & #proposalCreations .~ [(0,verifiedProposal)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                -- proposal.
                (emptyOptionsBuilderModel & #proposalCreations .~ 
                  [(0,verifiedProposal & #deposit .~ minUTxOValue1)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedProposal & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedProposal) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #optionsBuilderModel % #proposalUpdates % ix idx % _2 % #newProposal .~ 
              verifiedProposal
          & #txBuilderModel % #optionsBuilderModel % #targetProposalUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new proposal requires a deposit of: " <> display (verifiedProposal ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Proposal Purchase from Builder
  -----------------------------------------------
  RemoveSelectedProposalPurchase idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #proposalPurchases %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the selected proposal purchase
  -----------------------------------------------
  EditSelectedProposalPurchase modal -> case modal of
    StartProcess mNewPurchase ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (optionsBuilderEvent . EditSelectedProposalPurchase . ProcessResults) $ do
          let Config{network} = config
          (idx,newPurchase) <- fromJustOrAppError "options purchase mTarget is Nothing" mNewPurchase

          -- There should be two results, the first is for the premium payment output and the
          -- second is for the new Active contract UTxO.
          minUTxOValues <- 
            calculateMinUTxOValue 
              network 
              (txBuilderModel ^? #parameters % _Just % _1) 
              -- Use a blank optionsBuilderModel to calculate the minUTxOValues for the purchase.
              (emptyOptionsBuilderModel & #proposalPurchases .~ [(0,newPurchase)])

          (idx,) <$> fromRightOrAppError (updateOptionsPurchaseDeposits newPurchase minUTxOValues)
      ]
    ProcessResults info@(idx,verifiedPurchase) ->
      [ Model $ model 
          & #txBuilderModel % #optionsBuilderModel % #proposalPurchases % ix idx .~ info
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , createOptionsPurchaseDepositMsg verifiedPurchase
          ]
      ]

  -----------------------------------------------
  -- Remove Expired Close from Builder
  -----------------------------------------------
  RemoveSelectedExpiredOptionsClose idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #expiredCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Writer Address Update from Builder
  -----------------------------------------------
  RemoveSelectedWriterAddressUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #addressUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Writer Address Update
  -----------------------------------------------
  EditSelectedWriterAddressUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetAddressUpdate .~ 
          fmap (fmap toNewWriterAddressUpdate) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #optionsBuilderModel % #targetAddressUpdate .~ Nothing ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (optionsBuilderEvent . EditSelectedWriterAddressUpdate . AddResult) $ do
          (idx,newUpdate) <-
            fromJustOrAppError "Nothing set for `targetAddressUpdate`" $ 
              txBuilderModel ^. #optionsBuilderModel % #targetAddressUpdate

          verifiedUpdate <- fromRightOrAppError $ 
            verifyNewWriterAddressUpdate (config ^. #currentTime) newUpdate

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                  -- output.
                  (emptyOptionsBuilderModel & #addressUpdates .~ [(0,verifiedUpdate)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                -- output.
                (emptyOptionsBuilderModel & #addressUpdates .~ 
                  [(0,verifiedUpdate & #extraDeposit .~ minUTxOValue1)])

          return (idx, updateWriterAddressDeposit verifiedUpdate minUTxOValue)
      ]
    AddResult (idx,verifiedUpdate) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #optionsBuilderModel % #addressUpdates % ix idx % _2 .~ 
              verifiedUpdate
          & #txBuilderModel % #optionsBuilderModel % #targetAddressUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines
          [ "Successfully added to builder!"
          , ""
          , createWriterAddressDepositMsg verifiedUpdate
          ]
      ]

  -----------------------------------------------
  -- Remove Options Key Burn from builder
  -----------------------------------------------
  RemoveSelectedOptionsKeyBurn idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #keyBurns %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Options execution from builder
  -----------------------------------------------
  RemoveSelectedOptionsContractExecution idx ->
    [ Model $ model 
        & #txBuilderModel % #optionsBuilderModel % #contractExecutions %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]
