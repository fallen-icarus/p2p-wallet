module P2PWallet.GUI.EventHandler.TxBuilderEvent.AftermarketBuilderEvent
  ( 
    handleAftermarketBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Prelude

handleAftermarketBuilderEvent :: AppModel -> AftermarketBuilderEvent -> [AppEventResponse AppModel AppEvent]
handleAftermarketBuilderEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Remove Sale Creation from Builder
  -----------------------------------------------
  RemoveSelectedSaleCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #saleCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Sale Creation NFT
  -----------------------------------------------
  RemoveSaleCreationNft nft ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Remove Sale Update NFT
  -----------------------------------------------
  RemoveSaleUpdateNft nft ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #targetSaleUpdate % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Edit the Sale Creation
  -----------------------------------------------
  EditSelectedSaleCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation .~ 
          (mTarget & _Just % _2 %~ toNewSaleCreation reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedSaleCreation . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetSaleCreation is Nothing" $ 
            txBuilderModel ^. #aftermarketBuilderModel % #targetSaleCreation

          verifiedCreation <- fromRightOrAppError $ verifyNewSaleCreation tickerMap newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                  -- sale.
                  (emptyAftermarketBuilderModel & #saleCreations .~ [(0,verifiedCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                -- sale.
                (emptyAftermarketBuilderModel & #saleCreations .~ 
                  [(0,verifiedCreation & #deposit .~ minUTxOValue1)])

          -- Return the `SaleCreation` with the updated deposit field.
          return (idx, verifiedCreation & #deposit .~ minUTxOValue)
        ]
    AddResult newInfo@(idx,verifiedCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #saleCreations % ix idx .~ newInfo
          & #txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new aftermarket sale requires a deposit of: " <> display (verifiedCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Sale Close from Builder
  -----------------------------------------------
  RemoveSelectedSaleClose idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #saleCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Sale Update from Builder
  -----------------------------------------------
  RemoveSelectedSaleUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #saleUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Sale Update
  -----------------------------------------------
  EditSelectedSaleUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetSaleUpdate .~ 
          fmap (fmap (toNewSaleCreation reverseTickerMap . view #newSale)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetSaleUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedSaleUpdate . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetSaleUpdate is Nothing" $
            txBuilderModel ^. #aftermarketBuilderModel % #targetSaleUpdate

          verifiedCreation <- fromRightOrAppError $ verifyNewSaleCreation tickerMap newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                  -- sale.
                  (emptyAftermarketBuilderModel & #saleCreations .~ [(0,verifiedCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                -- sale.
                (emptyAftermarketBuilderModel & #saleCreations .~ 
                  [(0,verifiedCreation & #deposit .~ minUTxOValue1)])

          -- Return the `AskCreation` with the updated deposit field.
          return (idx, verifiedCreation & #deposit .~ minUTxOValue)
      ]
    AddResult (idx,verifiedSale) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #saleUpdates % ix idx % _2 % #newSale .~ 
              verifiedSale
          & #txBuilderModel % #aftermarketBuilderModel % #targetSaleUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new sale requires a deposit of: " <> display (verifiedSale ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Loan Key Spot from Builder
  -----------------------------------------------
  RemoveSelectedLoanKeySpotPurchase idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #loanKeySpotPurchases %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Loan Key Spot Purchase
  -----------------------------------------------
  EditSelectedLoanKeySpotPurchase modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeySpotPurchase .~ 
          fmap (over _2 toNewLoanKeySpotPurchase) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeySpotPurchase .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedLoanKeySpotPurchase . AddResult) $ do
          (idx,newPurchase) <- fromJustOrAppError "targetLoanKeySpotPurchase is Nothing" $
            txBuilderModel ^. #aftermarketBuilderModel % #targetLoanKeySpotPurchase

          verifiedPurchase <- fromRightOrAppError $ 
            verifyNewLoanKeySpotPurchase (config ^. #currentTime) newPurchase

          -- There will be three outputs for this action: the collateral output, the lender
          -- output with the new Key NFT, and the payment output for the spot. The first value in
          -- the list is for the collateral output, the second is for the key NFT, and the last
          -- output is for the spot payment.
          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. The whole txBuilderModel
            -- is needed since the lenderAddressUpdates are also needed from the loanBuilderModel.
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #loanKeySpotPurchases .~ [(0,verifiedPurchase)])

          updatedVerifiedPurchase <- 
            fromRightOrAppError $ updateLoanKeySpotPurchaseDeposits verifiedPurchase minValues

          return (idx, updatedVerifiedPurchase)
      ]
    AddResult (idx,verifiedPurchase) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeySpotPurchases % ix idx % _2 .~ verifiedPurchase
          & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeySpotPurchase .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully updated builder!"
          , createLoanKeySpotPurchaseDepositMsg verifiedPurchase
          ]
      ]

  -----------------------------------------------
  -- Remove Bid Creation NFT
  -----------------------------------------------
  RemoveBidCreationNft nft ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #targetBidCreation % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Edit the Bid Creation
  -----------------------------------------------
  EditSelectedBidCreation modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetBidCreation .~ 
          (mTarget & _Just % _2 %~ toNewBidCreation (config ^. #timeZone) reverseTickerMap)
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetBidCreation .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedBidCreation . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetBidCreation is Nothing" $ 
            txBuilderModel ^. #aftermarketBuilderModel % #targetBidCreation

          let Config{currentTime,timeZone} = config

          verifiedCreation <- 
            fromRightOrAppError $ verifyNewBidCreation currentTime timeZone tickerMap newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                  -- bid.
                  (emptyAftermarketBuilderModel & #bidCreations .~ [(0,verifiedCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                -- bid.
                (emptyAftermarketBuilderModel & #bidCreations .~ 
                  [(0,verifiedCreation & #deposit .~ minUTxOValue1)])

          -- Return the `BidCreation` with the updated deposit field.
          return (idx, verifiedCreation & #deposit .~ minUTxOValue)
        ]
    AddResult newInfo@(idx,verifiedCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #bidCreations % ix idx .~ newInfo
          & #txBuilderModel % #aftermarketBuilderModel % #targetBidCreation .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new bid requires a deposit of: " <> display (verifiedCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Bid Creation from Builder
  -----------------------------------------------
  RemoveSelectedBidCreation idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #bidCreations %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Bid Close from Builder
  -----------------------------------------------
  RemoveSelectedBidClose idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #bidCloses %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Bid Update from Builder
  -----------------------------------------------
  RemoveSelectedBidUpdate idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #bidUpdates %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Bid Update NFT
  -----------------------------------------------
  RemoveBidUpdateNft nft ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Edit the Bid Update
  -----------------------------------------------
  EditSelectedBidUpdate modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate .~ 
          -- (mTarget & _Just % _2 %~ toNewBidCreation (config ^. #timeZone) reverseTickerMap)
          fmap (fmap (toNewBidCreation (config ^. #timeZone) reverseTickerMap . view #newBid)) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedBidUpdate . AddResult) $ do
          (idx,newCreation) <- fromJustOrAppError "targetBidUpdate is Nothing" $ 
            txBuilderModel ^. #aftermarketBuilderModel % #targetBidUpdate

          let Config{currentTime,timeZone} = config

          verifiedCreation <- 
            fromRightOrAppError $ verifyNewBidCreation currentTime timeZone tickerMap newCreation

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                  -- bid.
                  (emptyAftermarketBuilderModel & #bidCreations .~ [(0,verifiedCreation)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                -- bid.
                (emptyAftermarketBuilderModel & #bidCreations .~ 
                  [(0,verifiedCreation & #deposit .~ minUTxOValue1)])

          -- Return the `BidCreation` with the updated deposit field.
          return (idx, verifiedCreation & #deposit .~ minUTxOValue)
        ]
    AddResult (idx,verifiedCreation) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #bidUpdates % ix idx % _2 % #newBid .~ 
              verifiedCreation
          & #txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Task $ return $ Alert $ unlines
          [ "Successfully updated builder!"
          , ""
          , "The new bid requires a deposit of: " <> display (verifiedCreation ^. #deposit)
          ]
      ]

  -----------------------------------------------
  -- Remove Claim Bid Acceptance from Builder
  -----------------------------------------------
  RemoveSelectedClaimBidAcceptance idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #claimBidAcceptances %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Claim Bid Acceptance
  -----------------------------------------------
  EditSelectedClaimBidAcceptance modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetClaimBidAcceptance .~ mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetClaimBidAcceptance .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedClaimBidAcceptance . AddResult) $ do
          (idx,verifiedAcceptance) <- fromJustOrAppError "targetClaimBidAcceptance is Nothing" $ 
            txBuilderModel ^. #aftermarketBuilderModel % #targetClaimBidAcceptance

          -- There should only be one output in the `TxBody` for this action. The calculation must
          -- be done twice because the datum must be updated with the minUTxOValue as well.
          minUTxOValue <- do
            minUTxOValue1 <- 
              fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
                calculateMinUTxOValue 
                  (config ^. #network) 
                  (txBuilderModel ^? #parameters % _Just % _1) 
                  -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                  -- bid.
                  (emptyAftermarketBuilderModel & #claimBidAcceptances .~ [(0,verifiedAcceptance)])

            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank aftermarketBuilderModel to calculate the minUTxOValue for the new
                -- bid.
                (emptyAftermarketBuilderModel & #claimBidAcceptances .~ 
                  [(0,updateClaimBidAcceptanceDeposit verifiedAcceptance minUTxOValue1)])

          -- Return the `SaleCreation` with the updated deposit field.
          return (idx, updateClaimBidAcceptanceDeposit verifiedAcceptance minUTxOValue)
        ]
    AddResult newInfo@(idx,verifiedAcceptance) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #claimBidAcceptances % ix idx .~ newInfo
          & #txBuilderModel % #aftermarketBuilderModel % #targetClaimBidAcceptance .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully updated builder!"
          , createClaimBidAcceptanceDepositMsg verifiedAcceptance
          ]
      ]

  -----------------------------------------------
  -- Remove LoanKeyBidClaim from Builder
  -----------------------------------------------
  RemoveSelectedLoanKeyBidClaim idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #loanKeyBidClaims %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Loan Key Bid Claim
  -----------------------------------------------
  EditSelectedLoanKeyBidClaim modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeyBidClaim .~ 
          fmap (over _2 toNewLoanKeyAcceptedBidClaim) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeyBidClaim .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedLoanKeyBidClaim . AddResult) $ do
          (idx,newClaim) <- fromJustOrAppError "targetLoanKeyBidClaim is Nothing" $
            txBuilderModel ^. #aftermarketBuilderModel % #targetLoanKeyBidClaim

          verifiedClaim <- fromRightOrAppError $ 
            verifyNewLoanKeyAcceptedBidClaim (config ^. #currentTime) newClaim

          -- There will be three outputs for this action: the collateral output, the lender
          -- output with the new Key NFT, and the payment output for the spot. The first value in
          -- the list is for the collateral output, the second is for the key NFT, and the last
          -- output is for the spot payment.
          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. The whole txBuilderModel
            -- is needed since the lenderAddressUpdates are also needed from the loanBuilderModel.
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #loanKeyBidClaims .~ [(0,verifiedClaim)])

          updatedVerifiedClaim <- 
            fromRightOrAppError $ updateLoanKeyAcceptedBidClaimDeposits verifiedClaim minValues

          return (idx, updatedVerifiedClaim)
      ]
    AddResult (idx,verifiedClaim) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeyBidClaims % ix idx % _2 .~ verifiedClaim
          & #txBuilderModel % #aftermarketBuilderModel % #targetLoanKeyBidClaim .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully updated builder!"
          , createLoanKeyAcceptedBidClaimDepositMsg verifiedClaim
          ]
      ]

  -----------------------------------------------
  -- Remove Options Key Spot Purchase from Builder
  -----------------------------------------------
  RemoveSelectedOptionsKeySpotPurchase idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #optionsKeySpotPurchases %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Options Key Spot Purchase
  -----------------------------------------------
  EditSelectedOptionsKeySpotPurchase modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeySpotPurchase .~ 
          fmap (over _2 toNewOptionsKeySpotPurchase) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeySpotPurchase .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedOptionsKeySpotPurchase . AddResult) $ do
          let MarketWallet{alias,network} = aftermarketModel ^. #selectedWallet

          (idx,newPurchase) <- fromJustOrAppError "targetOptionsKeySpotPurchase is Nothing" $
            txBuilderModel ^. #aftermarketBuilderModel % #targetOptionsKeySpotPurchase

          verifiedPurchase <- fromRightOrAppError $ 
            verifyNewOptionsKeySpotPurchase network alias (config ^. #currentTime) newPurchase

          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. The whole txBuilderModel
            -- is needed since the lenderAddressUpdates are also needed from the loanBuilderModel.
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #optionsKeySpotPurchases .~ [(0,verifiedPurchase)])

          updatedVerifiedPurchase <- 
            fromRightOrAppError $ updateOptionsKeySpotPurchaseDeposits verifiedPurchase minValues

          return (idx, updatedVerifiedPurchase)
      ]
    AddResult (idx,verifiedPurchase) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeySpotPurchases % ix idx % _2 .~ verifiedPurchase
          & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeySpotPurchase .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully updated builder!"
          , createOptionsKeySpotPurchaseDepositMsg verifiedPurchase
          ]
      ]

  -----------------------------------------------
  -- Remove Spot Bid Acceptance from Builder
  -----------------------------------------------
  RemoveSelectedSpotBidAcceptance idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #spotBidAcceptances %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Remove Options Key Accepted Bid Claim
  -----------------------------------------------
  RemoveSelectedOptionsKeyBidClaim idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #optionsKeyBidClaims %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

  -----------------------------------------------
  -- Edit the Options Key Bid Claim
  -----------------------------------------------
  EditSelectedOptionsKeyBidClaim modal -> case modal of
    StartAdding mTarget ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeyBidClaim .~ 
          fmap (over _2 toNewOptionsKeyAcceptedBidClaim) mTarget
      ]
    CancelAdding ->
      [ Model $ model & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeyBidClaim .~ Nothing ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (aftermarketBuilderEvent . EditSelectedOptionsKeyBidClaim . AddResult) $ do
          let MarketWallet{alias,network} = aftermarketModel ^. #selectedWallet

          (idx,newClaim) <- fromJustOrAppError "targetOptionsKeyBidClaim is Nothing" $
            txBuilderModel ^. #aftermarketBuilderModel % #targetOptionsKeyBidClaim

          verifiedClaim <- fromRightOrAppError $ 
            verifyNewOptionsKeyAcceptedBidClaim (config ^. #currentTime) alias network newClaim

          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. 
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #optionsKeyBidClaims .~ [(0,verifiedClaim)])

          updatedVerifiedClaim <- 
            fromRightOrAppError $ updateOptionsKeyAcceptedBidClaimDeposits verifiedClaim minValues

          return (idx, updatedVerifiedClaim)
      ]
    AddResult (idx,verifiedClaim) ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeyBidClaims % ix idx % _2 .~ verifiedClaim
          & #txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeyBidClaim .~ Nothing
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully updated builder!"
          , createOptionsKeyAcceptedBidClaimDepositMsg verifiedClaim
          ]
      ]

  -----------------------------------------------
  -- Remove Bid Unlock
  -----------------------------------------------
  RemoveSelectedBidUnlock idx ->
    [ Model $ model 
        & #txBuilderModel % #aftermarketBuilderModel % #bidUnlocks %~ removeAction idx
        & #txBuilderModel %~ balanceTx
    , Task $ return $ Alert "Successfully removed from builder!"
    ]

