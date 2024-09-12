module P2PWallet.GUI.EventHandler.OptionsEvent.WriterEvent
  ( 
    handleWriterEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleWriterEvent :: AppModel -> OptionsWriterEvent -> [AppEventResponse AppModel AppEvent]
handleWriterEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeOptionsWriterScene newScene -> 
    [ Model $ model & #optionsModel % #writerModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Add new proposal creation.
  -----------------------------------------------
  CreateNewOptionsProposal modal -> case modal of
    StartAdding _ -> 
      let mPaymentWallet = maybeHead $ model ^. #knownWallets % #paymentWallets
       in case mPaymentWallet of
            Nothing -> 
              [ Event $ Alert 
                  "You must first track a payment wallet before you can create an options proposal."
              ]
            Just paymentWallet ->
              [ Model $ model
                  & #optionsModel % #writerModel % #newProposalCreation ?~ 
                      createNewProposalCreation 
                        (config ^. #network)
                        (optionsModel ^. #selectedWallet)
                        paymentWallet
              ]
    CancelAdding -> 
      [ Model $ model 
          & #optionsModel % #writerModel % #newProposalCreation .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (OptionsEvent . OptionsWriterEvent . CreateNewOptionsProposal . AddResult) $ do
          newCreation <- fromJustOrAppError "newProposalCreation is Nothing" $ 
            optionsModel ^. #writerModel % #newProposalCreation

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
          return $ verifiedProposal & #deposit .~ minUTxOValue
      ]
    AddResult verifiedProposal ->
      -- Get the index for the new proposal creation.
      let newIdx = length $ txBuilderModel ^. #optionsBuilderModel % #proposalCreations 
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #optionsBuilderModel % #proposalCreations %~ 
                  flip snoc (newIdx,verifiedProposal)
              & #optionsModel % #writerModel % #newProposalCreation .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This options proposal requires a deposit of: " <> display (verifiedProposal ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Add Proposal Close to Builder
  -----------------------------------------------
  AddSelectedProposalClose optionsUTxO ->
    let OptionsWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
          optionsModel ^. #selectedWallet
        newInput = 
          optionsUTxOToProposalClose network alias stakeCredential stakeKeyDerivation optionsUTxO
     in case processNewProposalClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Add Selected Proposal Update to Builder
  -----------------------------------------------
  AddSelectedProposalUpdate modal -> case modal of
    StartAdding mTarget ->
      let OptionsWallet{alias} = optionsModel ^. #selectedWallet
          Config{network,timeZone} = config
          payToAddress = maybe "" (either (const "") fst . plutusToBech32 network) 
                       $ mTarget >>= optionsUTxOPaymentAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          mStartingWallet = case mTargetWallet of
            Nothing -> maybeHead $ knownWallets ^. #paymentWallets
            x -> x
          toNewCreation = optionsUTxOToNewProposalCreation 
            network
            alias 
            reverseTickerMap 
            (fromMaybe def mStartingWallet)
            timeZone
          newProposalUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in case mStartingWallet of
            Just _ -> 
              [ Model $ model
                  & #optionsModel % #writerModel % #newProposalUpdate .~ newProposalUpdate
              ]
            Nothing ->
              [ Event $ Alert "You must first add a payment wallet under the Home page." ]
    CancelAdding ->
      [ Model $ model
          & #optionsModel % #writerModel % #newProposalUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (OptionsEvent . OptionsWriterEvent . AddSelectedProposalUpdate . AddResult) $ do
          let OptionsWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                optionsModel ^. #selectedWallet

          (utxoToClose@OptionsUTxO{utxoRef},newCreation) <- 
            fromJustOrAppError "newProposalUpdate is Nothing" $ 
              optionsModel ^. #writerModel % #newProposalUpdate

          verifiedProposal <- fromRightOrAppError $
            verifyNewProposalCreation 
              reverseTickerMap 
              tickerMap 
              (config ^. #timeZone) 
              (config ^. #currentTime) 
              newCreation

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This proposal UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $
                  txBuilderModel ^. #optionsBuilderModel % #proposalCloses
              , map (view $ _2 % #oldProposal % #utxoRef) $
                  txBuilderModel ^. #optionsBuilderModel % #proposalUpdates
              , map (view $ _2 % #proposalUTxO % #utxoRef) $
                  txBuilderModel ^. #optionsBuilderModel % #proposalPurchases
              ])

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

          -- Return the `ProposalUpdate` with the updated deposit field.
          return $ ProposalUpdate
            { oldProposal = 
                optionsUTxOToProposalClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newProposal = verifiedProposal & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedUpdate ->
      -- Get the index for the new proposal update.
      let newIdx = length $ txBuilderModel ^. #optionsBuilderModel % #proposalUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #optionsBuilderModel % #proposalUpdates %~ 
                  flip snoc (newIdx,verifiedUpdate)
              & #optionsModel % #writerModel % #newProposalUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "The new proposal requires a deposit of: " <> 
                  display (verifiedUpdate ^. #newProposal % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetOpenProposalsFilters -> 
    [ Model $ model 
        & #optionsModel % #writerModel % #proposalsFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetActiveContractsFilters -> 
    [ Model $ model 
        & #optionsModel % #writerModel % #activesFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckOpenProposalsFilterModel ->
    case checkOpenProposalsFilterModel tickerMap $ optionsModel ^. #writerModel % #proposalsFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #optionsModel % #writerModel % #showProposalFilter .~ True -- keep it open
        , Event $ Alert err
        ]
  CheckActiveContractsFilterModel ->
    case checkActiveContractsFilterModel tickerMap $ optionsModel ^. #writerModel % #activesFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #optionsModel % #writerModel % #showActivesFilter .~ True -- keep it open
        , Event $ Alert err
        ]

  -----------------------------------------------
  -- Add Expired Close to Builder
  -----------------------------------------------
  AddSelectedExpiredOptionsClose optionsUTxO ->
    let OptionsWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
          optionsModel ^. #selectedWallet
        newInput = optionsUTxOToExpiredOptionsClose 
          network 
          alias 
          stakeCredential 
          stakeKeyDerivation 
          optionsUTxO
     in case processNewExpiredClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Change Writer Payment Address
  -----------------------------------------------
  UpdateWriterPaymentAddress modal -> case modal of
    StartAdding mOptionsUTxO -> 
      let OptionsWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
            optionsModel ^. #selectedWallet
          newInput = 
            createNewWriterAddressUpdate network alias stakeCredential stakeKeyDerivation $ 
              fromMaybe def mOptionsUTxO
       in [ Model $ model 
              & #optionsModel % #writerModel % #newWriterAddressUpdate ?~ newInput
          ]
    CancelAdding -> 
      [ Model $ model 
          & #optionsModel % #writerModel % #newWriterAddressUpdate .~ Nothing 
      ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (OptionsEvent . OptionsWriterEvent . UpdateWriterPaymentAddress . AddResult) $ do
          newUpdate <- fromJustOrAppError "newWriterAddressUpdate is Nothing" $ 
            optionsModel ^. #writerModel % #newWriterAddressUpdate

          -- Verify that the contract is not already being updated.
          fromRightOrAppError $
            maybeToLeft () $ "This payment address is already being updated." <$
              find (== newUpdate ^. #optionsUTxO)
                (map (view $ _2 % #optionsUTxO) $ 
                  txBuilderModel ^. #optionsBuilderModel % #addressUpdates)

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
                  -- proposal.
                  (emptyOptionsBuilderModel & #addressUpdates .~ [(0,verifiedUpdate)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank optionsBuilderModel to calculate the minUTxOValue for the new
                -- proposal.
                (emptyOptionsBuilderModel & #addressUpdates .~ 
                  [(0,verifiedUpdate & #extraDeposit .~ minUTxOValue1)])

          return $ updateWriterAddressDeposit verifiedUpdate minUTxOValue
      ]
    AddResult verifiedUpdate ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #optionsModel % #writerModel % #newWriterAddressUpdate .~ Nothing
          -- Add the new update with a dummy index.
          & #txBuilderModel % #optionsBuilderModel % #addressUpdates %~ 
              flip snoc (0,verifiedUpdate)
          -- Sort the updates by the active UTxO. This is required to generate 
          -- the outputs in the proper order.
          & #txBuilderModel % #optionsBuilderModel % #addressUpdates %~ 
              sortOn (view $ _2 % #optionsUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #optionsBuilderModel % #addressUpdates %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines $ intersperse "" $ filter (/= "")
          [ "Successfully added to builder!"
          , createWriterAddressDepositMsg verifiedUpdate
          ]
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new proposal close and add it to the builder. Balance the transaction after.
processNewProposalClose :: ProposalClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewProposalClose u@ProposalClose{utxoRef} model@TxBuilderModel{optionsBuilderModel} = do
  let OptionsBuilderModel{..} = optionsBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This proposal UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) proposalCloses
      , map (view $ _2 % #oldProposal % #utxoRef) proposalUpdates
      , map (view $ _2 % #proposalUTxO % #utxoRef) proposalPurchases
      ])

  -- Get the input's new index.
  let newIdx = length proposalCloses

  -- Add the new close to the end of the list of proposal closes.
  return $ balanceTx $ model 
    & #optionsBuilderModel % #proposalCloses %~ flip snoc (newIdx,u)

-- | Validate the new expired close and add it to the builder. Balance the transaction after.
processNewExpiredClose :: ExpiredOptionsClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewExpiredClose u@ExpiredOptionsClose{utxoRef} model@TxBuilderModel{optionsBuilderModel} = do
  let OptionsBuilderModel{..} = optionsBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This active UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) expiredCloses
      ])

  -- Get the input's new index.
  let newIdx = length expiredCloses

  -- Add the new close to the end of the list of expired closes.
  return $ balanceTx $ model 
    & #optionsBuilderModel % #expiredCloses %~ flip snoc (newIdx,u)
