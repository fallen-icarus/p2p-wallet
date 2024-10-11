module P2PWallet.GUI.EventHandler.AftermarketEvent.SellerEvent
  ( 
    handleSellerEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleSellerEvent :: AppModel -> AftermarketSellerEvent -> [AppEventResponse AppModel AppEvent]
handleSellerEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeAftermarketSellerScene newScene -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Inspect Batch
  -----------------------------------------------
  InspectAftermarketSale saleUTxO -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedSale ?~ saleUTxO 
    , Event $ AftermarketEvent $ LookupKeyInfo (False,saleUTxO)
    ]
  CloseInspectedAftermarketSale -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedSale .~ Nothing ]

  -----------------------------------------------
  -- Add Sale Close to Builder
  -----------------------------------------------
  AddSelectedSaleClose saleUTxO ->
    let MarketWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
          aftermarketModel ^. #selectedWallet
        newInput = 
          aftermarketUTxOToSaleClose network alias stakeCredential stakeKeyDerivation saleUTxO
     in case processNewSaleClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Add Selected Sale Update to Builder
  -----------------------------------------------
  AddSelectedSaleUpdate modal -> case modal of
    StartAdding mTarget ->
      let marketWallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
          payToAddress = maybe "" (either (const "") fst . plutusToBech32 network) 
                       $ mTarget >>= aftermarketUTxOSellerAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          mStartingWallet = case mTargetWallet of
            Nothing -> maybeHead $ knownWallets ^. #paymentWallets
            x -> x
          toNewCreation = 
            aftermarketUTxOToNewSaleCreation reverseTickerMap marketWallet (fromMaybe def mStartingWallet)
          newSaleUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in case mStartingWallet of
            Just _ -> 
              [ Model $ model
                  & #aftermarketModel % #sellerModel % #newSaleUpdate .~ newSaleUpdate
              -- Lookup the Key Info if they have not been synced yet.
              , Event $ AftermarketEvent $ LookupKeyInfo (False, fromMaybe def mTarget)
              ]
            Nothing ->
              [ Event $ Alert "You must first add a payment wallet under the Home page." ]
    CancelAdding ->
      [ Model $ model
          & #aftermarketModel % #sellerModel % #newSaleUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketSellerEvent . AddSelectedSaleUpdate . AddResult) $ do
          let MarketWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                aftermarketModel ^. #selectedWallet

          (utxoToClose@AftermarketUTxO{utxoRef},newCreation) <- 
            fromJustOrAppError "newSaleUpdate is Nothing" $ 
              aftermarketModel ^. #sellerModel % #newSaleUpdate

          verifiedCreation <- fromRightOrAppError $ verifyNewSaleCreation tickerMap newCreation

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This sale UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $
                  txBuilderModel ^. #aftermarketBuilderModel % #saleCloses
              , map (view $ _2 % #oldSale % #utxoRef) $
                  txBuilderModel ^. #aftermarketBuilderModel % #saleUpdates
              ])

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

          -- Return the `SaleUpdate` with the updated deposit field.
          return $ SaleUpdate
            { oldSale = 
                aftermarketUTxOToSaleClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newSale = verifiedCreation & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedUpdate ->
      -- Get the index for the new sale update.
      let newIdx = length $ txBuilderModel ^. #aftermarketBuilderModel % #saleUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #aftermarketBuilderModel % #saleUpdates %~ 
                  flip snoc (newIdx,verifiedUpdate)
              & #aftermarketModel % #sellerModel % #newSaleUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "The new sale requires a deposit of: " <> 
                  display (verifiedUpdate ^. #newSale % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Remove Sale Update NFT
  -----------------------------------------------
  RemoveSellerSaleUpdateNft nft ->
    [ Model $ model 
        & #aftermarketModel % #sellerModel % #newSaleUpdate % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]


  -----------------------------------------------
  -- Add an NFT in an open sale to the Home batch for creating a new sale with it
  -----------------------------------------------
  AddNftToHomeBatch nft ->
    case maybeHead $ homeModel ^. #queuedNFTs of
      Nothing -> 
        [ Model $ model 
            & #homeModel % #queuedNFTs %~ flip snoc nft
            & #homeModel % #queuedNFTs %~ sort
            & #homeModel % #queuedNFTs %~ ordNub -- remove duplicates
        , Event $ Alert "Successfully added to batch." 
        ]
      Just currentAsset -> 
        if currentAsset ^. #policyId == nft ^. #policyId then
          [ Model $ model 
              & #homeModel % #queuedNFTs %~ flip snoc nft
              & #homeModel % #queuedNFTs %~ sort
              & #homeModel % #queuedNFTs %~ ordNub -- remove duplicates
          , Event $ Alert "Successfully added to batch." 
          ]
        else
          [ Event $ Alert "All NFTs in the batch must have the same policy id." ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetOpenSalesFilters -> 
    [ Model $ model 
        & #aftermarketModel % #sellerModel % #salesFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckOpenSalesFilterModel ->
    case checkOpenSalesFilterModel $ aftermarketModel ^. #sellerModel % #salesFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #aftermarketModel % #sellerModel % #showSaleFilter .~ True -- keep it open
        , Event $ Alert err
        ]

  -----------------------------------------------
  -- Inspecting Seller Loan
  -----------------------------------------------
  InspectSellerLoanHistory loanId -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedLoan ?~ loanId 
    , Event $ case Map.lookup loanId (lendingModel ^. #cachedLoanHistories) of
        Nothing -> LendingEvent $ LookupLoanHistories $ StartProcess $ Just [loanId]
        Just _ -> AppInit
    ]
  CloseInspectedSellerLoanHistory -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedLoan .~ Nothing ]

  -----------------------------------------------
  -- Inspecting Borrower Info
  -----------------------------------------------
  InspectSellerBorrowerInformation borrower@(borrowerId,_) -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedBorrower ?~ borrower 
    , Event $ case Map.lookup borrowerId (lendingModel ^. #cachedBorrowerInfo) of
        Nothing -> LendingEvent $ LookupBorrowerInformation $ StartProcess $ Just borrower
        Just _ -> AppInit
    ]
  CloseInspectedSellerBorrowerInformation -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedBorrower .~ Nothing ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new sale close and add it to the builder. Balance the transaction after.
processNewSaleClose :: SaleClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewSaleClose u@SaleClose{utxoRef} model@TxBuilderModel{aftermarketBuilderModel} = do
  let AftermarketBuilderModel{..} = aftermarketBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This sale UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) saleCloses
      , map (view $ _2 % #oldSale % #utxoRef) saleUpdates
      ])

  -- Get the input's new index.
  let newIdx = length saleCloses

  -- Add the new close to the end of the list of sale closes.
  return $ balanceTx $ model 
    & #aftermarketBuilderModel % #saleCloses %~ flip snoc (newIdx,u)
