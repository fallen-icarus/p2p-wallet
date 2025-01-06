module P2PWallet.GUI.EventHandler.AftermarketEvent.BuyerEvent
  ( 
    handleBuyerEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

handleBuyerEvent :: AppModel -> AftermarketBuyerEvent -> [AppEventResponse AppModel AppEvent]
handleBuyerEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeAftermarketBuyerScene newScene -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Set the new desired policy id
  -----------------------------------------------
  SetNewSalePolicyId modal -> case modal of
    StartAdding _ ->
      let oldPolicy = aftermarketModel ^. #buyerModel % #selectedPolicyId
          newPolicy = maybe "" display oldPolicy
       in [ Model $ model
              & #aftermarketModel % #buyerModel % #newPolicyId .~ newPolicy
              & #aftermarketModel % #buyerModel % #choosingPolicyId .~ True
          ]
    CancelAdding ->
      [ Model $ model
          & #aftermarketModel % #buyerModel % #newPolicyId .~ ""
          & #aftermarketModel % #buyerModel % #choosingPolicyId .~ False
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . SetNewSalePolicyId . AddResult) $ do
          let AftermarketBuyerModel{newPolicyId,nftType} = aftermarketModel ^. #buyerModel
          case nftType of
            LoanKey -> return Loans.activeBeaconCurrencySymbol
            OptionsKey -> return Options.activeBeaconCurrencySymbol
            OtherNft -> 
              if newPolicyId == "" then
                throwIO $ AppError "Policy id field left empty."
              else 
                fromJustOrAppError ("Not a valid policy id: '" <> newPolicyId <> "'") $ 
                  CurrencySymbol <$> parseHex newPolicyId
      ]
    AddResult targetPolicy ->
      [ Model $ model 
          & #aftermarketModel % #buyerModel % #choosingPolicyId .~ False
          & #aftermarketModel % #buyerModel % #newPolicyId .~ ""
          & #aftermarketModel % #buyerModel % #selectedPolicyId ?~ targetPolicy
      , Task $ do
          -- Only sync the sales for the new policy id if it has not been synced yet.
          -- Users can manually resync if necessary.
          return $ case Map.lookup targetPolicy $ aftermarketModel ^. #cachedSales of
            Nothing -> AftermarketEvent $ SyncAftermarketSales $ StartProcess $ Just targetPolicy
            Just _ -> AppInit
      ]

  -----------------------------------------------
  -- Inspect Batch
  -----------------------------------------------
  InspectAftermarketBuyerSale saleUTxO -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedSale ?~ saleUTxO 
    , Event $ AftermarketEvent $ LookupKeyInfo (False,saleUTxO)
    ]
  CloseInspectedAftermarketBuyerSale -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedSale .~ Nothing ]

  -----------------------------------------------
  -- Inspect Seller
  -----------------------------------------------
  InspectSellerInformation sellerAddr -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedSeller ?~ sellerAddr
    , Event $ case Map.lookup sellerAddr (aftermarketModel ^. #cachedSellerInfo) of
        Nothing -> AftermarketEvent $ LookupSellerInfo $ StartProcess $ Just sellerAddr
        Just _ -> AppInit
    ]
  CloseInspectedSellerInformation -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedSeller .~ Nothing ]

  -----------------------------------------------
  -- Inspecting Buyer Loan
  -----------------------------------------------
  InspectBuyerLoanHistory loanId -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedLoan ?~ loanId 
    , Event $ case Map.lookup loanId (lendingModel ^. #cachedLoanHistories) of
        Nothing -> LendingEvent $ LookupLoanHistories $ StartProcess $ Just [loanId]
        Just _ -> AppInit
    ]
  CloseInspectedBuyerLoanHistory -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedLoan .~ Nothing ]

  -----------------------------------------------
  -- Inspecting Borrower Info
  -----------------------------------------------
  InspectBuyerBorrowerInformation borrower@(borrowerId,_) -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBorrower ?~ borrower 
    , Event $ case Map.lookup borrowerId (lendingModel ^. #cachedBorrowerInfo) of
        Nothing -> LendingEvent $ LookupBorrowerInformation $ StartProcess $ Just borrower
        Just _ -> AppInit
    ]
  CloseInspectedBuyerBorrowerInformation -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBorrower .~ Nothing ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetAllSalesFilters -> 
    [ Model $ model 
        & #aftermarketModel % #buyerModel % #salesFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Purchase Loan Key Spot
  -----------------------------------------------
  PurchaseLoanKeySpot modal -> case modal of
    StartAdding mTargets ->
      let (aftermarketUTxO,loanUTxOs) = fromMaybe (def,[]) mTargets
          wallet@PaymentWallet{network} = 
            fromMaybe def $ maybeHead $ knownWallets ^. #paymentWallets
          MarketWallet{alias} = aftermarketModel ^. #selectedWallet
          Config{currentTime} = config
          activeLoanUTxOs = 
            filter (maybe False (> toPlutusTime currentTime) . loanUTxOExpiration) loanUTxOs
          expiredLoanUTxOs = 
            filter (maybe False (<= toPlutusTime currentTime) . loanUTxOExpiration) loanUTxOs
          newAddressUpdates = 
            map (createNewLenderAddressUpdate network wallet alias) activeLoanUTxOs
          newClaims = 
            map (loanUTxOToExpiredClaim network alias Nothing Nothing currentTime) expiredLoanUTxOs
          newPurchase = NewLoanKeySpotPurchase
            { spotPurchase = aftermarketUTxOToSpotPurchase network aftermarketUTxO
            , newPaymentWallet = wallet
            , lenderAddressUpdates = newAddressUpdates
            , expiredClaims = newClaims
            }
       in [ Model $ model 
              & #aftermarketModel % #buyerModel % #newLoanKeySpotPurchase ?~ newPurchase
              & #aftermarketModel % #buyerModel % #showSpotPurchaseLenderAddressWidget .~ 
                  (newAddressUpdates /= [])
          , if null newAddressUpdates then 
              -- Skip getting the new lender address.
              Event $ AftermarketEvent $ AftermarketBuyerEvent $ PurchaseLoanKeySpot ConfirmAdding
            else 
              -- Get the new lender address.
              Event AppInit
          ]
    CancelAdding ->
      [ Model $ model 
          & #aftermarketModel % #buyerModel % #newLoanKeySpotPurchase .~ Nothing 
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . PurchaseLoanKeySpot . AddResult) $ do
          newPurchase <- fromJustOrAppError "newLoanKeySpotPurchase is Nothing" $ 
            aftermarketModel ^. #buyerModel % #newLoanKeySpotPurchase

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This Spot UTxO is already being purchased.") $
            find (== (newPurchase ^. #spotPurchase % #saleUTxO % #utxoRef)) $
              map (view $ _2 % #spotPurchase % #saleUTxO % #utxoRef) $
                txBuilderModel ^. #aftermarketBuilderModel % #loanKeySpotPurchases

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

          fromRightOrAppError $ updateLoanKeySpotPurchaseDeposits verifiedPurchase minValues
      ]
    AddResult verifiedPurchase ->
      [ Model $ model
          & #waitingStatus % #addingToBuilder .~ False
          & #aftermarketModel % #buyerModel % #newLoanKeySpotPurchase .~ Nothing
          & #aftermarketModel % #buyerModel % #showSpotPurchaseLenderAddressWidget .~ False
          -- Add the new update with a dummy index.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeySpotPurchases %~ 
              flip snoc (0,verifiedPurchase)
          -- Sort the updates by the spot UTxO. This is required to generate 
          -- the spot payment outputs in the proper order. The loan address updates are handled
          -- separately.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeySpotPurchases %~ 
              sortOn (view $ _2 % #spotPurchase % #saleUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeySpotPurchases %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully added to builder!"
          , createSpotPurchaseImmediateExpiredLoansClaimMsg verifiedPurchase
          , createLoanKeySpotPurchaseDepositMsg verifiedPurchase
          ]
      ]

  -----------------------------------------------
  -- Add Selected Bid Creation to Builder
  -----------------------------------------------
  CreateBid modal -> case modal of
    StartAdding mTarget ->
      let Config{currentTime,timeZone} = config
          marketWallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
          paymentWallet = fromMaybe def $ maybeHead $ knownWallets ^. #paymentWallets
          newCreation = 
            aftermarketUTxOToNewBidCreation 
              currentTime 
              timeZone 
              reverseTickerMap
              network
              marketWallet 
              paymentWallet
              (fromMaybe def mTarget)
       in [ Model $ model
              & #aftermarketModel % #buyerModel % #newBidCreation ?~ newCreation
          ]
    CancelAdding ->
      [ Model $ model
          & #aftermarketModel % #buyerModel % #newBidCreation .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . CreateBid . AddResult) $ do
          let Config{currentTime,timeZone} = config

          newCreation <- fromJustOrAppError "newBidCreation is Nothing" $ 
            aftermarketModel ^. #buyerModel % #newBidCreation

          verifiedCreation@BidCreation{deposit} <- fromRightOrAppError $ 
            verifyNewBidCreation currentTime timeZone tickerMap newCreation

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

          return $ verifiedCreation & #deposit .~ 
            -- Only replace the user specified deposit if the minUTxOValue is larger.
            if minUTxOValue > deposit then minUTxOValue else deposit
      ]
    AddResult verifiedCreation ->
      -- Get the index for the new bid creation.
      let newIdx = length $ txBuilderModel ^. #aftermarketBuilderModel % #bidCreations
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #aftermarketBuilderModel % #bidCreations %~ 
                  flip snoc (newIdx,verifiedCreation)
              & #aftermarketModel % #buyerModel % #newBidCreation .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "The new bid requires a deposit of: " <> 
                  display (verifiedCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Remove Bid Creation NFT
  -----------------------------------------------
  RemoveBuyerBidCreationNft nft ->
    [ Model $ model 
        & #aftermarketModel % #buyerModel % #newBidCreation % _Just % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Inspect Batch
  -----------------------------------------------
  InspectAftermarketBid bidUTxO -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBid ?~ bidUTxO 
    , Event $ AftermarketEvent $ LookupKeyInfo (False,bidUTxO)
    ]
  CloseInspectedAftermarketBid -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBid .~ Nothing ]

  -----------------------------------------------
  -- Add Bid Close to Builder
  -----------------------------------------------
  AddSelectedBidClose bidUTxO ->
    let MarketWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
          aftermarketModel ^. #selectedWallet
        newInput = 
          aftermarketUTxOToBidClose network alias stakeCredential stakeKeyDerivation bidUTxO
     in case processNewBidClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Add Selected Bid Update to Builder
  -----------------------------------------------
  AddSelectedBidUpdate modal -> case modal of
    StartAdding mTarget ->
      let marketWallet@MarketWallet{network} = aftermarketModel ^. #selectedWallet
          mPaymentWallet = maybeHead $ knownWallets ^. #paymentWallets
          Config{currentTime,timeZone} = config
          toNewCreation = 
            aftermarketUTxOToNewBidCreation 
              currentTime 
              timeZone 
              reverseTickerMap
              network
              marketWallet 
              (fromMaybe def mPaymentWallet)
          newBidUpdate = 
            (,) <$> mTarget
                <*> (toNewCreation <$> mTarget)
       in case mPaymentWallet of
            Just _ -> 
              [ Model $ model
                  & #aftermarketModel % #buyerModel % #newBidUpdate .~ newBidUpdate
              -- Lookup the Key Info if they have not been synced yet.
              , Event $ AftermarketEvent $ LookupKeyInfo (False, fromMaybe def mTarget)
              ]
            Nothing ->
              [ Event $ Alert "You must first add a payment wallet under the Home page." ]
    CancelAdding ->
      [ Model $ model
          & #aftermarketModel % #buyerModel % #newBidUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . AddSelectedBidUpdate . AddResult) $ do
          let MarketWallet{network,alias,stakeCredential,stakeKeyDerivation} = 
                aftermarketModel ^. #selectedWallet
              Config{currentTime,timeZone} = config

          (utxoToClose@AftermarketUTxO{utxoRef},newCreation) <- 
            fromJustOrAppError "newBidUpdate is Nothing" $ 
              aftermarketModel ^. #buyerModel % #newBidUpdate

          verifiedCreation@BidCreation{deposit} <- fromRightOrAppError $ 
            verifyNewBidCreation currentTime timeZone tickerMap newCreation

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This sale UTxO is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $
                  txBuilderModel ^. #aftermarketBuilderModel % #bidCloses
              , map (view $ _2 % #oldBid % #utxoRef) $
                  txBuilderModel ^. #aftermarketBuilderModel % #bidUpdates
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

          -- Return the `BidUpdate` with the updated deposit field.
          return $ BidUpdate
            { oldBid = 
                aftermarketUTxOToBidClose network alias stakeCredential stakeKeyDerivation utxoToClose
            , newBid = verifiedCreation & #deposit .~
                -- Only replace the user specified deposit if the minUTxOValue is larger.
                if minUTxOValue > deposit then minUTxOValue else deposit
            }
      ]
    AddResult verifiedUpdate ->
      -- Get the index for the new sale update.
      let newIdx = length $ txBuilderModel ^. #aftermarketBuilderModel % #bidUpdates
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #aftermarketBuilderModel % #bidUpdates %~ 
                  flip snoc (newIdx,verifiedUpdate)
              & #aftermarketModel % #buyerModel % #newBidUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "The new bid requires a deposit of: " <> 
                  display (verifiedUpdate ^. #newBid % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Remove Bid Update NFT
  -----------------------------------------------
  RemoveBuyerBidUpdateNft nft ->
    [ Model $ model 
        & #aftermarketModel % #buyerModel % #newBidUpdate % _Just % _2 % #nfts %~ 
            filter (/=nft)
    ]

  -----------------------------------------------
  -- Claim accepted loan key bid
  -----------------------------------------------
  ClaimAcceptedLoanKeyBid modal -> case modal of
    StartAdding mTargets ->
      let (aftermarketUTxO,loanUTxOs) = fromMaybe (def,[]) mTargets
          wallet@PaymentWallet{network} = 
            fromMaybe def $ maybeHead $ knownWallets ^. #paymentWallets
          marketWallet@MarketWallet{alias} = aftermarketModel ^. #selectedWallet
          Config{currentTime} = config
          activeLoanUTxOs = 
            filter (maybe False (> toPlutusTime currentTime) . loanUTxOExpiration) loanUTxOs
          expiredLoanUTxOs = 
            filter (maybe False (<= toPlutusTime currentTime) . loanUTxOExpiration) loanUTxOs
          newAddressUpdates = 
            map (createNewLenderAddressUpdate network wallet alias) activeLoanUTxOs
          newClaims = 
            map (loanUTxOToExpiredClaim network alias Nothing Nothing currentTime) expiredLoanUTxOs
          newBidClaim = NewLoanKeyAcceptedBidClaim
            { bidClaim = aftermarketUTxOToAcceptedBidClaim network marketWallet aftermarketUTxO
            , newPaymentWallet = wallet
            , lenderAddressUpdates = newAddressUpdates
            , expiredClaims = newClaims
            }
       in [ Model $ model 
              & #aftermarketModel % #buyerModel % #newLoanKeyBidClaim ?~ newBidClaim
              & #aftermarketModel % #buyerModel % #showBidClaimLenderAddressWidget .~ 
                  (newAddressUpdates /= [])
          , if null newAddressUpdates then 
              -- Skip getting the new lender address.
              Event $ AftermarketEvent $ AftermarketBuyerEvent $ ClaimAcceptedLoanKeyBid ConfirmAdding
            else 
              -- Get the new lender address.
              Event AppInit
          ]
    CancelAdding ->
      [ Model $ model 
          & #aftermarketModel % #buyerModel % #newLoanKeyBidClaim .~ Nothing 
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . ClaimAcceptedLoanKeyBid . AddResult) $ do
          newClaim <- fromJustOrAppError "newLoanKeyBidClaim is Nothing" $ 
            aftermarketModel ^. #buyerModel % #newLoanKeyBidClaim

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This Bid UTxO is already being claimed.") $
            find (== (newClaim ^. #bidClaim % #bidUTxO % #utxoRef)) $
              map (view $ _2 % #bidClaim % #bidUTxO % #utxoRef) $
                txBuilderModel ^. #aftermarketBuilderModel % #loanKeyBidClaims

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

          fromRightOrAppError $ updateLoanKeyAcceptedBidClaimDeposits verifiedClaim minValues
      ]
    AddResult verifiedClaim ->
      [ Model $ model
          & #waitingStatus % #addingToBuilder .~ False
          & #aftermarketModel % #buyerModel % #newLoanKeyBidClaim .~ Nothing
          & #aftermarketModel % #buyerModel % #showBidClaimLenderAddressWidget .~ False
          -- Add the new claim with a dummy index.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeyBidClaims %~ 
              flip snoc (0,verifiedClaim)
          -- Sort the updates by the spot UTxO. This is required to generate 
          -- the spot payment outputs in the proper order. The loan address updates are handled
          -- separately.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeyBidClaims %~ 
              sortOn (view $ _2 % #bidClaim % #bidUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #aftermarketBuilderModel % #loanKeyBidClaims %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully added to builder!"
          , createBidClaimImmediateExpiredLoansClaimMsg verifiedClaim
          , createLoanKeyAcceptedBidClaimDepositMsg verifiedClaim
          ]
      ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetBidTxFilters -> 
    let newDefault = def & #dateRange % _1 ?~ addDays (-30) (config ^. #currentDay) in
    [ Model $ model 
        & #aftermarketModel % #buyerModel % #txFilterModel .~ newDefault
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetOwnBidsFilters -> 
    [ Model $ model 
        & #aftermarketModel % #buyerModel % #bidsFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Check Filters
  -----------------------------------------------
  CheckBidTxFilters ->
    let BidTxFilterModel{nftType,policyId} = aftermarketModel ^. #buyerModel % #txFilterModel in
    case nftType of
      Just OtherNft ->
        if isJust $ parseHex policyId then [Event AppInit] else
          [ Model $ model
              & #aftermarketModel % #buyerModel % #showTransactionFilter .~ True -- keep it open
          , Event $ Alert $ "Could not parse policy id: '" <> policyId <> "'"
          ]
      _ -> [Event AppInit]
  CheckOwnBidsFilters ->
    case checkOwnBidsFilterModel $ aftermarketModel ^. #buyerModel % #bidsFilterModel of
      Right () -> [Event AppInit]
      Left err ->
        [ Model $ model
            & #aftermarketModel % #buyerModel % #showBidFilter .~ True -- keep it open
        , Event $ Alert err
        ]

  -----------------------------------------------
  -- Inspecting Transactions
  -----------------------------------------------
  InspectBidTransaction tx -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBidTransaction ?~ tx ]
  CloseInspectedBidTransaction -> 
    [ Model $ model & #aftermarketModel % #buyerModel % #inspectedBidTransaction .~ Nothing ]

  -----------------------------------------------
  -- Purchase Options Key Spot
  -----------------------------------------------
  PurchaseOptionsKeySpot modal -> case modal of
    StartAdding mTargets ->
      let (aftermarketUTxO,optionsUTxOs) = fromMaybe (def,[]) mTargets
          Config{network} = config
          newPurchase = NewOptionsKeySpotPurchase
            { spotPurchase = aftermarketUTxOToSpotPurchase network aftermarketUTxO
            , contracts = map (False,) optionsUTxOs
            }
       in [ Model $ model 
              & #aftermarketModel % #buyerModel % #newOptionsKeySpotPurchase ?~ newPurchase
          ]
    CancelAdding ->
      [ Model $ model 
          & #aftermarketModel % #buyerModel % #newOptionsKeySpotPurchase .~ Nothing 
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . PurchaseOptionsKeySpot . AddResult) $ do
          let MarketWallet{alias,network} = aftermarketModel ^. #selectedWallet

          newPurchase <- fromJustOrAppError "newOptionsKeySpotPurchase is Nothing" $ 
            aftermarketModel ^. #buyerModel % #newOptionsKeySpotPurchase

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This Spot UTxO is already being purchased.") $
            find (== (newPurchase ^. #spotPurchase % #saleUTxO % #utxoRef)) $
              map (view $ _2 % #spotPurchase % #saleUTxO % #utxoRef) $
                txBuilderModel ^. #aftermarketBuilderModel % #optionsKeySpotPurchases

          verifiedPurchase <- fromRightOrAppError $ 
            verifyNewOptionsKeySpotPurchase network alias (config ^. #currentTime) newPurchase

          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. 
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #optionsKeySpotPurchases .~ [(0,verifiedPurchase)])

          fromRightOrAppError $ updateOptionsKeySpotPurchaseDeposits verifiedPurchase minValues
      ]
    AddResult verifiedPurchase ->
      [ Model $ model
          & #waitingStatus % #addingToBuilder .~ False
          & #aftermarketModel % #buyerModel % #newOptionsKeySpotPurchase .~ Nothing
          -- Add the new purchase with a dummy index.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeySpotPurchases %~ 
              flip snoc (0,verifiedPurchase)
          -- Sort the purchases by the spot UTxO. This is required to generate 
          -- the spot payment outputs in the proper order. The executions will be handled
          -- separately.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeySpotPurchases %~ 
              sortOn (view $ _2 % #spotPurchase % #saleUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeySpotPurchases %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully added to builder!"
          , createOptionsKeySpotPurchaseDepositMsg verifiedPurchase
          ]
      ]

  -----------------------------------------------
  -- Claim an accepted options key bid
  -----------------------------------------------
  ClaimAcceptedOptionsKeyBid modal -> case modal of
    StartAdding mTargets ->
      let (aftermarketUTxO,optionsUTxOs) = fromMaybe (def,[]) mTargets
          wallet = aftermarketModel ^. #selectedWallet
          Config{network} = config
          newClaim = NewOptionsKeyAcceptedBidClaim
            { bidClaim = aftermarketUTxOToAcceptedBidClaim network wallet aftermarketUTxO
            , contracts = map (False,) optionsUTxOs
            }
       in [ Model $ model 
              & #aftermarketModel % #buyerModel % #newOptionsKeyAcceptedBidClaim ?~ newClaim
          ]
    CancelAdding ->
      [ Model $ model 
          & #aftermarketModel % #buyerModel % #newOptionsKeyAcceptedBidClaim .~ Nothing 
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AftermarketEvent . AftermarketBuyerEvent . ClaimAcceptedOptionsKeyBid . AddResult) $ do
          let MarketWallet{alias,network} = aftermarketModel ^. #selectedWallet

          newClaim <- fromJustOrAppError "newOptionsKeyAcceptedBidClaim is Nothing" $ 
            aftermarketModel ^. #buyerModel % #newOptionsKeyAcceptedBidClaim

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This Bid UTxO is already being claim.") $
            find (== (newClaim ^. #bidClaim % #bidUTxO % #utxoRef)) $
              map (view $ _2 % #bidClaim % #bidUTxO % #utxoRef) $
                txBuilderModel ^. #aftermarketBuilderModel % #optionsKeyBidClaims

          verifiedClaim <- fromRightOrAppError $ 
            verifyNewOptionsKeyAcceptedBidClaim (config ^. #currentTime) alias network newClaim

          minValues <- calculateMinUTxOValue 
            (config ^. #network) 
            (txBuilderModel ^? #parameters % _Just % _1) 
            -- Use a blank txBuilderModel to calculate the minUTxOValues. 
            ((def::TxBuilderModel) & #aftermarketBuilderModel % #optionsKeyBidClaims .~ [(0,verifiedClaim)])

          fromRightOrAppError $ updateOptionsKeyAcceptedBidClaimDeposits verifiedClaim minValues
      ]
    AddResult verifiedClaim ->
      [ Model $ model
          & #waitingStatus % #addingToBuilder .~ False
          & #aftermarketModel % #buyerModel % #newOptionsKeyAcceptedBidClaim .~ Nothing
          -- Add the new claim with a dummy index.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeyBidClaims %~ 
              flip snoc (0,verifiedClaim)
          -- Sort the claims by the bid UTxO. This is required to generate 
          -- the payment outputs in the proper order. The executions will be handled
          -- separately.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeyBidClaims %~ 
              sortOn (view $ _2 % #bidClaim % #bidUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #aftermarketBuilderModel % #optionsKeyBidClaims %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ Alert $ unlines $ intersperse "" $ filter (/="")
          [ "Successfully added to builder!"
          , createOptionsKeyAcceptedBidClaimDepositMsg verifiedClaim
          ]
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new bid close and add it to the builder. Balance the transaction after.
processNewBidClose :: BidClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewBidClose u@BidClose{utxoRef} model@TxBuilderModel{aftermarketBuilderModel} = do
  let AftermarketBuilderModel{..} = aftermarketBuilderModel

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This bid UTxO is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) bidCloses
      , map (view $ _2 % #oldBid % #utxoRef) bidUpdates
      ])

  -- Get the input's new index.
  let newIdx = length bidCloses

  -- Add the new close to the end of the list of bid closes.
  return $ balanceTx $ model 
    & #aftermarketBuilderModel % #bidCloses %~ flip snoc (newIdx,u)
