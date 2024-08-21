module P2PWallet.GUI.EventHandler.LendingEvent.LendEvent
  ( 
    handleLendEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.SyncLoans
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Prelude

handleLendEvent :: AppModel -> LendEvent -> [AppEventResponse AppModel AppEvent]
handleLendEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeLendScene newScene -> 
    [ Model $ model & #lendingModel % #lendModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Set the new desired ask configuration.
  -----------------------------------------------
  InitializeLoanAskConfiguration modal -> case modal of
    StartAdding _ ->
      let newAskCfg = maybe def (toNewLoanAskConfiguration reverseTickerMap) $ 
            lendingModel ^. #lendModel % #selectedLoanAskConfiguration
       in [ Model $ model
              & #lendingModel % #lendModel % #newLoanAskConfiguration ?~ newAskCfg
          ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #lendModel % #newLoanAskConfiguration .~ Nothing
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (LendingEvent . LendEvent . InitializeLoanAskConfiguration . AddResult) $ do
          -- `InitializeLoanAskConfiguration StartAdding` is not always called so this can still
          -- be `Nothing`.
          let newAskCfg = fromMaybe def $ lendingModel ^. #lendModel % #newLoanAskConfiguration

          fromRightOrAppError $ verifyNewLoanAskConfiguration tickerMap newAskCfg
      ]
    AddResult verifiedAskConfig ->
      [ Model $ model 
          -- Save the user supplied newLoanAskConfiguration for quick editing.
          & #lendingModel % #lendModel % #requestsFilterModel % #newLoanAskConfiguration .~
              fromMaybe def (lendingModel ^. #lendModel % #newLoanAskConfiguration)
          & #lendingModel % #lendModel % #newLoanAskConfiguration .~ Nothing
          & #lendingModel % #lendModel % #selectedLoanAskConfiguration ?~ verifiedAskConfig
      , Task $ do
          -- Only sync the loan asks for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case Map.lookup verifiedAskConfig $ lendingModel ^. #lendModel % #cachedLoanAsks of
            Nothing -> return $ LendingEvent $ LendEvent $ SyncLoanAsks StartProcess
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Update the desired ask configuration.
  -----------------------------------------------
  UpdateLoanAskConfiguration modal -> case modal of
    StartProcess ->
      [ Task $ runActionOrAlert (LendingEvent . LendEvent . UpdateLoanAskConfiguration . ProcessResults) $ do
          let newAskCfg = lendingModel ^. #lendModel % #requestsFilterModel % #newLoanAskConfiguration

          fromRightOrAppError $ verifyNewLoanAskConfiguration tickerMap newAskCfg
      ]
    ProcessResults verifiedAskConfig ->
      [ Model $ model 
          & #lendingModel % #lendModel % #selectedLoanAskConfiguration ?~ verifiedAskConfig
      , Task $ do
          -- Only sync the loan asks for the configuration if it has not been synced yet.
          -- Users can manually resync if necessary.
          case Map.lookup verifiedAskConfig $ lendingModel ^. #lendModel % #cachedLoanAsks of
            Nothing -> return $ LendingEvent $ LendEvent $ SyncLoanAsks StartProcess
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Sync Loan Asks
  -----------------------------------------------
  SyncLoanAsks modal -> case modal of
    StartProcess ->
      [ Model $ model & #waitingStatus % #syncingLoanAsks .~ True
      , Task $ runActionOrAlert (LendingEvent . LendEvent . SyncLoanAsks . ProcessResults) $ do
          askConfig <- fromJustOrAppError "selectedLoanAskConfiguration is Nothing" $ 
            lendingModel ^. #lendModel % #selectedLoanAskConfiguration

          syncLoanAsks (config ^. #network) askConfig $ lendingModel ^. #lendModel % #cachedLoanAsks
      ]
    ProcessResults newCachedLoanAsks ->
      [ Model $ model 
          & #waitingStatus % #syncingLoanAsks .~ False
          & #lendingModel % #lendModel % #cachedLoanAsks .~ newCachedLoanAsks
      ]

  -----------------------------------------------
  -- Create New Offer
  -----------------------------------------------
  CreateNewOffer modal -> case modal of
    StartAdding mAskUTxO ->
      let utxo = fromMaybe def mAskUTxO
          lenderWallet = model ^. #lendingModel % #selectedWallet
          mPaymentWallet = maybeHead $ model ^. #knownWallets % #paymentWallets
       in case mPaymentWallet of
            Nothing -> 
              [ Task $ runActionOrAlert (const AppInit) $ throwIO $ 
                  AppError "You must first track a payment wallet before you can create an offer."
              ]
            Just paymentWallet ->
              [ Model $ model
                  & #lendingModel % #lendModel % #newOfferCreation ?~ 
                      createNewOfferCreation reverseTickerMap utxo lenderWallet paymentWallet
              ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #lendModel % #newOfferCreation .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . LendEvent . CreateNewOffer . AddResult) $ do
          when (hasAskActions $ txBuilderModel ^. #loanBuilderModel) $
            throwIO $ AppError $ borrowAndLendError

          when ([] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances) $
            throwIO $ AppError $ acceptAndNegotiateError

          newOffer <- fromJustOrAppError "newOfferCreation is Nothing" $
            lendingModel ^. #lendModel % #newOfferCreation

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOffer

          fromRightOrAppError $ 
            checkIsSameLoanUserCredential 
              (verifiedOfferCreation ^. #lenderCredential)
              (txBuilderModel ^. #loanBuilderModel)

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferCreation` with the updated deposit field.
          return $ verifiedOfferCreation & #deposit .~ minUTxOValue
      ]
    AddResult verifiedOfferCreation ->
      -- Get the index for the new offer creation.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #offerCreations 
       in [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #offerCreations %~ 
                  flip snoc (newIdx,verifiedOfferCreation)
              & #txBuilderModel % #loanBuilderModel % #userCredential ?~
                  verifiedOfferCreation ^. #lenderCredential
              & #lendingModel % #lendModel % #newOfferCreation .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This offer requires a deposit of: " <> display (verifiedOfferCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Add Offer Close to Builder
  -----------------------------------------------
  AddSelectedOfferClose loanUTxO ->
    let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
        newInput = loanUTxOToOfferClose network alias stakeCredential stakeKeyDerivation loanUTxO
    in  case processNewOfferClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasAskActions $ txBuilderModel ^. #loanBuilderModel then
              [ Event $ Alert borrowAndLendError ]
            else if [] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances then
              [ Event $ Alert acceptAndNegotiateError ]
            else
              [ Model $ model & #txBuilderModel .~ newTxModel
              , Task $ return $ Alert "Successfully added to builder!"
              ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetOpenOffersFilters -> 
    [ Model $ model 
        & #lendingModel % #lendModel % #openOffersFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckOpenOffersFilterModel ->
    case checkOpenOffersFilterModel tickerMap $ lendingModel ^. #lendModel % #openOffersFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #lendingModel % #lendModel % #showOpenOffersFilter .~ True -- keep it open
        , Event $ Alert err
        ]

  -----------------------------------------------
  -- Add Selected Offer Update to Builder
  -----------------------------------------------
  AddSelectedOfferUpdate modal -> case modal of
    StartAdding mTarget ->
      let LoanWallet{alias,stakeCredential,stakeKeyDerivation} = lendingModel ^. #selectedWallet
          Config{network,currentTime} = config
          payToAddress = maybe "" (either (const "") fst . plutusToBech32 network) 
                       $ mTarget >>= loanUTxOLenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          mStartingWallet = case mTargetWallet of
            Nothing -> maybeHead $ knownWallets ^. #paymentWallets
            x -> x
          toNewCreation = loanUTxOToNewOfferCreation 
            currentTime
            network
            alias 
            stakeCredential 
            stakeKeyDerivation 
            reverseTickerMap 
            (fromMaybe def mStartingWallet)
          newOfferUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in case mStartingWallet of
            Just _ -> 
              [ Model $ model
                  & #lendingModel % #lendModel % #newOfferUpdate .~ newOfferUpdate
              ]
            Nothing ->
              [ Event $ Alert "You must first add a payment wallet under the Home page." ]
    CancelAdding ->
      [ Model $ model
          & #lendingModel % #lendModel % #newOfferUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (LendingEvent . LendEvent . AddSelectedOfferUpdate . AddResult) $ do
          when (hasAskActions $ txBuilderModel ^. #loanBuilderModel) $ 
            throwIO $ AppError $ borrowAndLendError

          when ([] /= txBuilderModel ^. #loanBuilderModel % #offerAcceptances) $
            throwIO $ AppError $ acceptAndNegotiateError

          let LoanWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                lendingModel ^. #selectedWallet

          (utxoToClose@LoanUTxO{utxoRef},newOffer) <- 
            fromJustOrAppError "newOfferUpdate is Nothing" $ lendingModel ^. #lendModel % #newOfferUpdate

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOffer

          fromRightOrAppError $ 
            checkIsSameLoanUserCredential 
              (verifiedOfferCreation ^. #lenderCredential)
              (txBuilderModel ^. #loanBuilderModel)

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This offer UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #offerCloses
              , map (view $ _2 % #oldOffer % #utxoRef) $ 
                  txBuilderModel ^. #loanBuilderModel % #offerUpdates
              ])

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferUpdate` with the updated deposit field.
          return $ OfferUpdate
            { oldOffer = loanUTxOToOfferClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newOffer = verifiedOfferCreation & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedOfferUpdate ->
      -- Get the index for the new offer update.
      let newIdx = length $ txBuilderModel ^. #loanBuilderModel % #offerUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #loanBuilderModel % #offerUpdates %~ 
                  flip snoc (newIdx,verifiedOfferUpdate)
              & #txBuilderModel % #loanBuilderModel % #userCredential ?~
                  verifiedOfferUpdate ^. #newOffer % #lenderCredential
              & #lendingModel % #lendModel % #newOfferUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This offer requires a deposit of: " <> 
                  display (verifiedOfferUpdate ^. #newOffer % #deposit)
              ]
          ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new ask close and add it to the builder. Balance the transaction after.
processNewOfferClose :: OfferClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewOfferClose u@OfferClose{utxoRef} model@TxBuilderModel{loanBuilderModel=LoanBuilderModel{..}} = do
  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This offer UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) offerCloses
      , map (view $ _2 % #oldOffer % #utxoRef) offerUpdates
      , map (view $ _2 % #offerUTxO % #utxoRef) offerAcceptances
      ])

  -- All actions must be for the same user.
  checkIsSameLoanUserCredential (u ^. #lenderCredential) (model ^. #loanBuilderModel)

  -- Get the input's new index.
  let newIdx = length offerCloses

  -- Add the new close to the end of the list of ask closes.
  return $ balanceTx $ model 
    & #loanBuilderModel % #offerCloses %~ flip snoc (newIdx,u)
    & #loanBuilderModel % #userCredential ?~ u ^. #lenderCredential
