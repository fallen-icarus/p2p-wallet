module P2PWallet.GUI.EventHandler.LendingEvent.BorrowEvent
  ( 
    handleBorrowEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleBorrowEvent :: AppModel -> BorrowEvent -> [AppEventResponse AppModel AppEvent]
handleBorrowEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeBorrowScene newScene -> 
    [ Model $ model & #lendingModel % #borrowModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Add new ask creation.
  -----------------------------------------------
  CreateNewAsk modal -> case modal of
    StartAdding _ -> 
      let LoanWallet{stakeCredential,loanAddress,alias,stakeKeyDerivation} = 
            lendingModel ^. #selectedWallet
          newAsk = 
            createNewAskCreation 
              (config ^. #network) 
              stakeCredential 
              stakeKeyDerivation 
              loanAddress 
              alias
       in [ Model $ model 
              & #lendingModel % #borrowModel % #newAskCreation ?~ newAsk
          ]
    CancelAdding -> 
      [ Model $ model 
          & #lendingModel % #borrowModel % #newAskCreation .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . BorrowEvent . CreateNewAsk . AddResult) $ do
          when (hasOfferActions $ txBuilderModel ^. #loanBuilderModel) $
            throwIO $ AppError $ borrowAndLendError

          when ([] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances) $
            throwIO $ AppError $ acceptAndNegotiateError

          verifiedAskCreation <- fromRightOrAppError $
            verifyNewAskCreation tickerMap $ fromMaybe def $ 
              lendingModel ^. #borrowModel % #newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return $ verifiedAskCreation & #deposit .~ minUTxOValue
      ]
    AddResult verifiedAskCreation ->
      -- Get the index for the new ask creation.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #askCreations 
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #askCreations %~ 
                  flip snoc (newIdx,verifiedAskCreation)
              & #lendingModel % #borrowModel % #newAskCreation .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This ask requires a deposit of: " <> display (verifiedAskCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Add Ask Close to Builder
  -----------------------------------------------
  AddSelectedAskClose loanUTxO ->
    let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
        newInput = loanUTxOToAskClose network alias stakeCredential stakeKeyDerivation loanUTxO
    in  case processNewAskClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOfferActions $ txBuilderModel ^. #loanBuilderModel then
              [ Event $ Alert borrowAndLendError ]
            else if [] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances then
              [ Event $ Alert acceptAndNegotiateError ]
            else
              [ Model $ model & #txBuilderModel .~ newTxModel
              , Task $ return $ Alert "Successfully added to builder!"
              ]

  -----------------------------------------------
  -- Add Selected Ask Update to Builder
  -----------------------------------------------
  AddSelectedAskUpdate modal -> case modal of
    StartAdding mTarget ->
      let LoanWallet{alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
          toNewCreation = loanUTxOToNewAskCreation 
            (config ^. #network) 
            alias 
            stakeCredential 
            stakeKeyDerivation 
            reverseTickerMap 
          newAskUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in [ Model $ model
              & #lendingModel % #borrowModel % #newAskUpdate .~ newAskUpdate
          ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #borrowModel % #newAskUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . BorrowEvent . AddSelectedAskUpdate . AddResult) $ do
          when (hasOfferActions $ txBuilderModel ^. #loanBuilderModel) $ 
            throwIO $ AppError $ borrowAndLendError

          when ([] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances) $
            throwIO $ AppError $ acceptAndNegotiateError

          let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                lendingModel ^. #selectedWallet
          (utxoToClose@LoanUTxO{utxoRef},newAsk) <- 
            fromJustOrAppError "Nothing set for `newAskUpdate`" $ 
              lendingModel ^. #borrowModel % #newAskUpdate

          verifiedAskCreation <- fromRightOrAppError $ verifyNewAskCreation tickerMap newAsk

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This ask UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #askCloses
              , map (view $ _2 % #oldAsk % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #askUpdates
              ])

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskUpdate` with the updated deposit field.
          return $ AskUpdate
            { oldAsk = loanUTxOToAskClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newAsk = verifiedAskCreation & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedAskUpdate ->
      -- Get the index for the new ask update.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #askUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #askUpdates %~ 
                  flip snoc (newIdx,verifiedAskUpdate)
              & #lendingModel % #borrowModel % #newAskUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This ask requires a deposit of: " <> display (verifiedAskUpdate ^. #newAsk % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckOpenAsksFilterModel ->
    case checkOpenAsksFilterModel tickerMap $ lendingModel ^. #borrowModel % #openAsksFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #lendingModel % #borrowModel % #showOpenAsksFilter .~ True -- keep it open
        , Task $ runActionOrAlert (const AppInit) $ throwIO $ AppError err
        ]

  CheckLenderOffersFilterModel ->
    case checkLenderOffersFilterModel tickerMap $ lendingModel ^. #borrowModel % #lenderOffersFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #lendingModel % #borrowModel % #showLenderOffersFilter .~ True -- keep it open
        , Task $ runActionOrAlert (const AppInit) $ throwIO $ AppError err
        ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetOpenAsksFilters -> 
    [ Model $ model 
        & #lendingModel % #borrowModel % #openAsksFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetLenderOffersFilters -> 
    [ Model $ model 
        & #lendingModel % #borrowModel % #lenderOffersFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Add Selected Offer Acceptance to Builder
  -----------------------------------------------
  AcceptLoanOffer modal -> case modal of
    ChooseOfferToAccept offerUTxO ->
      let usedAsks = map (view $ _2 % #askUTxO % #utxoRef) 
                   $ txBuilderModel ^. #loanBuilderModel % #offerAcceptances
          availableAsks = filter 
            (\x -> isJust (loanUTxOAskDatum x) && (x ^. #utxoRef) `notElem` usedAsks)
            (lendingModel ^. #selectedWallet % #utxos)
          currentOffersBeingSpent = map (view $ _2 % #offerUTxO % #utxoRef) 
                                  $ txBuilderModel ^. #loanBuilderModel % #offerAcceptances
       in if null availableAsks then
            [ Event $ Alert $ unwords 
                [ "There are no available loan asks to close. You must first create a new ask"
                , "before you can accept another offer."
                ]
            ]
          else if (offerUTxO ^. #utxoRef) `elem` currentOffersBeingSpent then
            [ Event $ Alert "This offer is already being accepted." ]
          else
            [ Model $ model
                & #lendingModel % #borrowModel % #offerAcceptanceScene .~ ChooseAskScene
                & #lendingModel % #borrowModel % #newOfferAcceptance ?~
                    createNewOfferAcceptance 
                      reverseTickerMap 
                      offerUTxO
                      (fromMaybe def $ maybeHead availableAsks)
                      (lendingModel ^. #selectedWallet)
            ]
    CancelAcceptance ->
      [ Model $ model
          & #lendingModel % #borrowModel % #newOfferAcceptance .~ Nothing
      ]
    ChooseAskToClose askUTxO ->
      [ Model $ model
          & #lendingModel % #borrowModel % #newOfferAcceptance % _Just % #askUTxO .~ askUTxO
          & #lendingModel % #borrowModel % #offerAcceptanceScene .~ SpecifyCollateralScene
      ]
    ReturnToChooseAskMenu ->
      [ Model $ model
          & #lendingModel % #borrowModel % #offerAcceptanceScene .~ ChooseAskScene
      ]
    ProcessAcceptance ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . BorrowEvent . AcceptLoanOffer . AddNewAcceptance) $ do
          let loanBuilderModel = txBuilderModel ^. #loanBuilderModel
          when (hasAskActions loanBuilderModel || hasOfferActions loanBuilderModel) $
            throwIO $ AppError $ acceptAndNegotiateError

          newOfferAcceptance <-
            fromJustOrAppError "Nothing set for `newOfferAcceptance`" $ 
              lendingModel ^. #borrowModel % #newOfferAcceptance

          verifiedOfferAcceptance <- fromRightOrAppError $
            verifyNewOfferAcceptance tickerMap (config ^. #currentTime) newOfferAcceptance

          -- There will be two outputs for this action: the collateral output and the lender
          -- output with the new Key NFT. The first value in the list is for the collateral
          -- output.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new acceptance.
                (emptyLoanBuilderModel & #offerAcceptances .~ [(0,verifiedOfferAcceptance)])

          return $ verifiedOfferAcceptance & #deposit .~ minUTxOValue
      ]
    AddNewAcceptance verifiedOfferAcceptance ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          -- Add the new acceptance with a dummy index.
          & #txBuilderModel % #loanBuilderModel % #offerAcceptances %~ 
              flip snoc (0,verifiedOfferAcceptance)
          -- Sort the acceptances by the offer UTxO. This is required to generate the outputs
          -- in the proper order.
          & #txBuilderModel % #loanBuilderModel % #offerAcceptances %~ 
              sortOn (view $ _2 % #offerUTxO % #utxoRef)
          -- Reindex the acceptances after sorting.
          & #txBuilderModel % #loanBuilderModel % #offerAcceptances %~ reIndex
          & #lendingModel % #borrowModel % #newOfferAcceptance .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully added to builder!"
          , ""
          , "This new collateral output requires a deposit of: " <> 
              display (verifiedOfferAcceptance ^. #deposit)
          ]
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new ask close and add it to the builder. Balance the transaction after.
processNewAskClose :: AskClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewAskClose u@AskClose{utxoRef} model@TxBuilderModel{loanBuilderModel=LoanBuilderModel{..}} = do
  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This ask UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) askCloses
      , map (view $ _2 % #oldAsk % #utxoRef) askUpdates
      , map (view $ _2 % #askUTxO % #utxoRef) offerAcceptances
      ])

  -- Get the input's new index.
  let newIdx = length askCloses

  -- Add the new close to the end of the list of ask closes.
  return $ balanceTx $ model & #loanBuilderModel % #askCloses %~ flip snoc (newIdx,u)
