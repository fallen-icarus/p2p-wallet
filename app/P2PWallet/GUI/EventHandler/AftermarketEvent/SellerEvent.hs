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
import P2PWallet.Plutus
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
  -- Inspect Batch
  -----------------------------------------------
  InspectAftermarketSellerBid bidUTxO -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedBid ?~ bidUTxO 
    , Event $ AftermarketEvent $ LookupKeyInfo (False,bidUTxO)
    ]
  CloseInspectedAftermarketSellerBid -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedBid .~ Nothing ]

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

  -----------------------------------------------
  -- Add Selected Claim Bid Acceptance to Builder
  -----------------------------------------------
  AddSelectedClaimBidAcceptance modal -> case modal of
    StartAdding mTarget ->
      let marketWallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
          mStartingWallet = maybeHead $ knownWallets ^. #paymentWallets
          Config{currentTime} = config
          newAcceptance = 
            aftermarketUTxOToClaimBidAcceptance 
              currentTime 
              network 
              marketWallet 
              (fromMaybe def mStartingWallet)
              (fromMaybe def mTarget)
       in case mStartingWallet of
            Just _ -> 
              [ Model $ model
                  & #aftermarketModel % #sellerModel % #newClaimBidAcceptance ?~ newAcceptance
              ]
            Nothing ->
              [ Event $ Alert "You must first add a payment wallet under the Home page." ]
    CancelAdding ->
      [ Model $ model
          & #aftermarketModel % #sellerModel % #newClaimBidAcceptance .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketSellerEvent . AddSelectedClaimBidAcceptance . AddResult) $ do
          verifiedAcceptance <- fromJustOrAppError "newClaimBidAcceptance is Nothing" $ 
              aftermarketModel ^. #sellerModel % #newClaimBidAcceptance

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This bid UTxO is already being spent.") $
            find (== (verifiedAcceptance ^. #bidUTxO % #utxoRef)) $
              map (view $ _2 % #bidUTxO % #utxoRef) $
                txBuilderModel ^. #aftermarketBuilderModel % #claimBidAcceptances

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

          -- Return the `ClaimBidAcceptances` with the updated deposit field.
          return $ updateClaimBidAcceptanceDeposit verifiedAcceptance minUTxOValue
      ]
    AddResult verifiedAcceptance ->
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #aftermarketModel % #sellerModel % #newClaimBidAcceptance .~ Nothing
          -- Add the new acceptance with a dummy index.
          & #txBuilderModel % #aftermarketBuilderModel % #claimBidAcceptances %~ 
              flip snoc (0,verifiedAcceptance)
          -- Sort the updates by the bid UTxO. This is required to generate 
          -- the bid outputs in the proper order. The loan address updates are handled
          -- separately.
          & #txBuilderModel % #aftermarketBuilderModel % #claimBidAcceptances %~ 
              sortOn (view $ _2 % #bidUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #aftermarketBuilderModel % #claimBidAcceptances %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully added to builder!"
          , createClaimBidAcceptanceDepositMsg verifiedAcceptance
          ]
      ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetSellerTxFilters -> 
    let newDefault = def & #dateRange % _1 ?~ addDays (-30) (config ^. #currentDay) in
    [ Model $ model 
        & #aftermarketModel % #sellerModel % #txFilterModel .~ newDefault
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetCurrentBidsFilters -> 
    [ Model $ model 
        & #aftermarketModel % #sellerModel % #bidsFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckSellerTxFilters ->
    let SellerTxFilterModel{nftType,policyId} = aftermarketModel ^. #sellerModel % #txFilterModel in
    case nftType of
      Just OtherNft ->
        if isJust $ parseHex policyId then [Event AppInit] else
          [ Model $ model
              & #aftermarketModel % #sellerModel % #showTransactionFilter .~ True -- keep it open
          , Event $ Alert $ "Could not parse policy id: '" <> policyId <> "'"
          ]
      _ -> [Event AppInit]
  CheckCurrentBidsFilters ->
    case checkCurrentBidsFilterModel $ aftermarketModel ^. #sellerModel % #bidsFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #aftermarketModel % #sellerModel % #showBidFilter .~ True -- keep it open
        , Event $ Alert err
        ]

  -----------------------------------------------
  -- Inspecting Transactions
  -----------------------------------------------
  InspectSellerTransaction tx -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedTransaction ?~ tx ]
  CloseInspectedSellerTransaction -> 
    [ Model $ model & #aftermarketModel % #sellerModel % #inspectedTransaction .~ Nothing ]

  -----------------------------------------------
  -- Add Spot Bid Acceptance to Builder
  -----------------------------------------------
  AddSelectedSpotBidAcceptance bidUTxO ->
    let wallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
        newInput = aftermarketUTxOToSpotBidAcceptance network wallet bidUTxO
     in case processNewSpotBidAcceptance newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Add Bid Unlock to Builder
  -----------------------------------------------
  AddSelectedBidUnlock bidUTxO ->
    let sellerWallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
        newInput = aftermarketUTxOToBidUnlock network sellerWallet bidUTxO
     in case processNewBidUnlock newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

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

-- | Validate the new sale close and add it to the builder. Balance the transaction after.
processNewSpotBidAcceptance :: SpotBidAcceptance -> TxBuilderModel -> Either Text TxBuilderModel
processNewSpotBidAcceptance u@SpotBidAcceptance{bidUTxO} model@TxBuilderModel{aftermarketBuilderModel} = do
  let AftermarketBuilderModel{..} = aftermarketBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This spot bid UTxO is already being spent." <$
    find (== bidUTxO ^. #utxoRef) (map (view $ _2 % #bidUTxO % #utxoRef) spotBidAcceptances)

  -- Add the new close to the end of the list of sale closes.
  return $ balanceTx $ model 
    -- Add the new acceptance with a dummy index.
    & #aftermarketBuilderModel % #spotBidAcceptances %~ flip snoc (0,u)
    -- Sort the acceptances by the bid UTxO. This is required to generate 
    -- the outputs in the proper order. 
    & #aftermarketBuilderModel % #spotBidAcceptances %~ 
        sortOn (view $ _2 % #bidUTxO % #utxoRef)
    -- Reindex after sorting.
    & #aftermarketBuilderModel % #spotBidAcceptances %~ reIndex

-- | Validate the new bid unlock and add it to the builder. Balance the transaction after.
processNewBidUnlock :: BidUnlock -> TxBuilderModel -> Either Text TxBuilderModel
processNewBidUnlock u@BidUnlock{bidUTxO} model@TxBuilderModel{aftermarketBuilderModel} = do
  let AftermarketBuilderModel{..} = aftermarketBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This bid UTxO is already being spent." <$
    find (== bidUTxO ^. #utxoRef) (map (view $ _2 % #bidUTxO % #utxoRef) bidUnlocks)

  -- Get the input's new index.
  let newIdx = length bidUnlocks

  -- Add the new close to the end of the list of sale closes.
  return $ balanceTx $ model 
    & #aftermarketBuilderModel % #bidUnlocks %~ flip snoc (newIdx,u)

