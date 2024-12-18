module P2PWallet.GUI.EventHandler.TxBuilderEvent.LoanBuilderEvent
  ( 
    handleLoanBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

handleLoanBuilderEvent :: AppModel -> LoanBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleLoanBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Remove Ask Creation from Builder
  -----------------------------------------------
  RemoveSelectedAskCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Increment the number of copies of that Ask creation
  -----------------------------------------------
  ChangeAskCreationCount idx newCount ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCreations % ix idx % _2 % #count .~ newCount
        & #txBuilderModel %~ balanceTx
    ]

  -----------------------------------------------
  -- Edit the Ask Creation
  -----------------------------------------------
  EditSelectedAskCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ 
          (mTarget & _Just % _2 %~ toNewAskCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedAskCreation . AddResult) $ do
          (idx,newAskCreation) <- fromJustOrAppError "targetAskCreation is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetAskCreation
          verifiedAskCreation <- fromRightOrAppError $
            verifyNewAskCreation tickerMap newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new ask.
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedAskCreation & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedAskCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #askCreations % ix idx .~ newInfo
          & #txBuilderModel % #loanBuilderModel % #targetAskCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new ask requires a deposit of: " <> display (verifiedAskCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Ask Close from Builder
  -----------------------------------------------
  RemoveSelectedAskClose idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Ask Update from Builder
  -----------------------------------------------
  RemoveSelectedAskUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #askUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Ask Update
  -----------------------------------------------
  EditSelectedAskUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ 
          fmap (fmap (toNewAskCreation reverseTickerMap . view #newAsk)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedAskUpdate . AddResult) $ do
          (idx,newAskCreation) <- fromJustOrAppError "targetAskUpdate is Nothing" $
            txBuilderModel ^. #loanBuilderModel % #targetAskUpdate

          verifiedAskCreation <- fromRightOrAppError $ verifyNewAskCreation tickerMap newAskCreation

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                (emptyLoanBuilderModel & #askCreations .~ [(0,verifiedAskCreation)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedAskCreation & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedAsk) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #askUpdates % ix idx % _2 % #newAsk .~ 
              verifiedAsk
          & #txBuilderModel % #loanBuilderModel % #targetAskUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new ask UTxO requires a deposit of: " <> display (verifiedAsk ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Offer Creation from Builder
  -----------------------------------------------
  RemoveSelectedOfferCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Offer Creation
  -----------------------------------------------
  EditSelectedOfferCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ 
          (mTarget & _Just % _2 %~ toNewOfferCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedOfferCreation . AddResult) $ do
          (idx,newOfferCreation) <- fromJustOrAppError "targetOfferCreation is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetOfferCreation

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOfferCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferCreation` with the updated deposit field.
          return (idx, verifiedOfferCreation & #deposit .~ minUTxOValue)
      ]
    AddResult newInfo@(idx,verifiedOfferCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #offerCreations % ix idx .~ newInfo
          & #txBuilderModel % #loanBuilderModel % #targetOfferCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new offer requires a deposit of: " <> display (verifiedOfferCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Offer Close from Builder
  -----------------------------------------------
  RemoveSelectedOfferClose idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Offer Update from Builder
  -----------------------------------------------
  RemoveSelectedOfferUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Offer Update
  -----------------------------------------------
  EditSelectedOfferUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ 
          fmap (fmap (toNewOfferCreation reverseTickerMap . view #newOffer)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedOfferUpdate . AddResult) $ do
          (idx,newOfferCreation) <- fromJustOrAppError "targetOfferUpdate is Nothing" $ 
            txBuilderModel ^. #loanBuilderModel % #targetOfferUpdate

          verifiedOfferCreation <- fromRightOrAppError $
            verifyNewOfferCreation reverseTickerMap tickerMap (config ^. #currentTime) newOfferCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                  (emptyLoanBuilderModel & #offerCreations .~ [(0,verifiedOfferCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new offer.
                (emptyLoanBuilderModel & #offerCreations .~ 
                  [(0,verifiedOfferCreation & #deposit .~ minUTxOValue1)])

          -- Return the `OfferCreation` with the updated deposit field.
          return (idx, verifiedOfferCreation & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedOffer) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #offerUpdates % ix idx % _2 % #newOffer .~ 
              verifiedOffer
          & #txBuilderModel % #loanBuilderModel % #targetOfferUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new offer UTxO requires a deposit of: " <> display (verifiedOffer ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Offer Acceptance from Builder
  -----------------------------------------------
  RemoveSelectedOfferAcceptance idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #offerAcceptances %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Offer Acceptance
  -----------------------------------------------
  EditSelectedOfferAcceptance modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferAcceptance .~ 
          fmap (fmap $ toNewOfferAcceptance reverseTickerMap) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetOfferAcceptance .~ Nothing ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedOfferAcceptance . AddResult) $ do
          (idx,newOfferAcceptance) <-
            fromJustOrAppError "Nothing set for `targetOfferAcceptance`" $ 
              txBuilderModel ^. #loanBuilderModel % #targetOfferAcceptance

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

          let updatedVerifiedOfferAcceptance = verifiedOfferAcceptance & #deposit .~ minUTxOValue

          fromRightOrAppError $ acceptanceAdaCollateralCheck updatedVerifiedOfferAcceptance

          return (idx, updatedVerifiedOfferAcceptance)
      ]
    AddResult (idx,verifiedOfferAcceptance) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #offerAcceptances % ix idx % _2 .~ 
              verifiedOfferAcceptance
          & #txBuilderModel % #loanBuilderModel % #targetOfferAcceptance .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully added to builder!"
          , ""
          , createAcceptanceDepositMsg verifiedOfferAcceptance
          ]
      ]

  -----------------------------------------------
  -- Remove Loan Payment from Builder
  -----------------------------------------------
  RemoveSelectedLoanPayment idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #loanPayments %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Loan Payment
  -----------------------------------------------
  EditSelectedLoanPayment modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetLoanPayment .~ 
          fmap (fmap $ toNewLoanPayment reverseTickerMap) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetLoanPayment .~ Nothing ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedLoanPayment . AddResult) $ do
          (idx,newLoanPayemnt) <-
            fromJustOrAppError "Nothing set for `targetLoanPayment`" $ 
              txBuilderModel ^. #loanBuilderModel % #targetLoanPayment

          verifiedNewPayment <- fromRightOrAppError $
            verifyNewLoanPayment reverseTickerMap tickerMap (config ^. #currentTime) newLoanPayemnt

          -- There will be two outputs for this action: the collateral output and the lender
          -- output with the new Key NFT. The first value in the list is for the lender
          -- output. When a partial payment is made, the second output will be the required
          -- collateral output.
          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new payment.
            (emptyLoanBuilderModel & #loanPayments .~ [(0,verifiedNewPayment)])

          updatedVerifiedPayment <- fromRightOrAppError $
            updateLoanPaymentDeposits minValues verifiedNewPayment reverseTickerMap

          fromRightOrAppError $ paymentAdaCollateralCheck updatedVerifiedPayment

          return (idx,updatedVerifiedPayment)
      ]
    AddResult (idx,verifiedNewPayment) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #loanPayments % ix idx % _2 .~ 
              verifiedNewPayment
          & #txBuilderModel % #loanBuilderModel % #targetLoanPayment .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines
          [ "Successfully added to builder!"
          , ""
          , createLoanPaymentDepositMsg verifiedNewPayment
          ]
      ]

  -----------------------------------------------
  -- Remove Interest Application from Builder
  -----------------------------------------------
  RemoveSelectedInterestApplication idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #interestApplications %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Expired Claim from Builder
  -----------------------------------------------
  RemoveSelectedExpiredClaim idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #expiredClaims %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Loan Key Burn from Builder
  -----------------------------------------------
  RemoveSelectedLoanKeyBurn idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #keyBurns %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Lender Address Update from Builder
  -----------------------------------------------
  RemoveSelectedLenderAddressUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #loanBuilderModel % #addressUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Lender Address Update
  -----------------------------------------------
  EditSelectedLenderAddressUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAddressUpdate .~ 
          fmap (fmap toNewLenderAddressUpdate) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #loanBuilderModel % #targetAddressUpdate .~ Nothing ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (loanBuilderEvent . EditSelectedLenderAddressUpdate . AddResult) $ do
          (idx,newUpdate) <-
            fromJustOrAppError "Nothing set for `targetAddressUpdate`" $ 
              txBuilderModel ^. #loanBuilderModel % #targetAddressUpdate

          verifiedUpdate <- fromRightOrAppError $ 
            verifyNewLenderAddressUpdate (config ^. #currentTime) newUpdate

          -- There will be two outputs for this action: the collateral output and the lender
          -- output with the new Key NFT. The first value in the list is for the collateral
          -- output. 
          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank loanBuilderModel to calculate the minUTxOValue for the new payment.
            (emptyLoanBuilderModel & #addressUpdates .~ [(0,verifiedUpdate)])

          updatedVerifiedUpdate <- fromRightOrAppError $ 
            updateLenderAddressDeposit verifiedUpdate minValues

          return (idx,updatedVerifiedUpdate)
      ]
    AddResult (idx,verifiedUpdate) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #loanBuilderModel % #addressUpdates % ix idx % _2 .~ 
              verifiedUpdate
          & #txBuilderModel % #loanBuilderModel % #targetAddressUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines
          [ "Successfully added to builder!"
          , ""
          , createLenderAddressDepositMsg verifiedUpdate
          ]
      ]

  -----------------------------------------------
  -- Set payment to outstanding balance
  -----------------------------------------------
  SetEditLoanPaymentToFullPayment ->
    let Loans.ActiveDatum{loanAsset,loanOutstanding} = 
          fromMaybe def $ 
            loanUTxOActiveDatum =<<
                txBuilderModel ^? #loanBuilderModel % #targetLoanPayment % _Just % _2 % #activeUTxO
        newPayment = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
     in [ Model $ model
           & #txBuilderModel % #loanBuilderModel % #targetLoanPayment % _Just % _2 % #paymentAmount .~
             showAssetBalance True reverseTickerMap newPayment
        ]

