module P2PWallet.GUI.EventHandler.TxBuilderEvent.AftermarketBuilderEvent
  ( 
    handleAftermarketBuilderEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
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
