module P2PWallet.GUI.EventHandler.HomeEvent
  ( 
    handleHomeEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Prelude

handleHomeEvent :: AppModel -> HomeEvent -> [AppEventResponse AppModel AppEvent]
handleHomeEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeHomeScene newScene -> 
    [ Model $ model & #homeModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowHomeMorePopup -> 
    [ Model $ model & #homeModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  -- Track the new payment wallet and also add the associated stake address if it isn't being
  -- tracked already.
  PairPaymentWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `pairing` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model 
          & #homeModel % #addingWallet .~ True -- Show widget.
          & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model 
          & #homeModel % #addingWallet .~ False 
          & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & #waitingStatus % #waitingOnDevice .~ True
      , Task $ runActionOrAlert (HomeEvent . PairPaymentWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def selectedProfile
              newWallet = homeModel ^. #newPaymentWallet

          -- Get the new payment id for the new entry into the payment_wallet table.
          paymentWalletId <- getNextPaymentWalletId databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and export the required keys.
          verifiedPaymentWallet <- 
            pairPaymentWallet network profile paymentWalletId newWallet $ knownWallets ^. #paymentWallets

          -- Add the new payment wallet to the database.
          insertPaymentWallet databaseFile verifiedPaymentWallet >>= fromRightOrAppError

          return verifiedPaymentWallet
      ]
    AddResult verifiedPaymentWallet ->
      [ Model $ model 
          & #knownWallets % #paymentWallets %~ flip snoc verifiedPaymentWallet
          & #waitingStatus % #waitingOnDevice .~ False
          & #homeModel % #addingWallet .~ False
          & #homeModel % #selectedWallet .~ verifiedPaymentWallet
          & #scene .~ HomeScene
      , Task $ return $ HomeEvent $ AddCorrespondingStakeWallet $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchPaymentWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `addingWallet` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model 
          & #homeModel % #addingWallet .~ True -- Show widget.
          & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model 
          & #homeModel % #addingWallet .~ False 
          & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    ConfirmAdding -> 
      -- Validate the information and generate the stake address, if any.
      [ Task $ runActionOrAlert (HomeEvent . WatchPaymentWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def selectedProfile
              newWallet = homeModel ^. #newPaymentWallet

          -- Get the new payment id for the new entry into the payment_wallet table.
          paymentWalletId <- getNextPaymentWalletId databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and extract the stake address, if any.
          verifiedPaymentWallet <- 
            watchPaymentWallet network profile paymentWalletId newWallet $ knownWallets ^. #paymentWallets

          -- Add the new payment wallet to the database.
          insertPaymentWallet databaseFile verifiedPaymentWallet >>= fromRightOrAppError

          return verifiedPaymentWallet
      ]
    AddResult verifiedPaymentWallet ->
      [ Model $ model 
          & #knownWallets % #paymentWallets %~ flip snoc verifiedPaymentWallet
          & #homeModel % #addingWallet .~ False
          & #homeModel % #selectedWallet .~ verifiedPaymentWallet
          & #scene .~ HomeScene
      , Task $ return $ HomeEvent $ AddCorrespondingStakeWallet $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Add the corresponding stake wallet when adding a payment wallet
  -----------------------------------------------
  AddCorrespondingStakeWallet modal -> case modal of
    StartProcess _ ->
      let PaymentWallet{..} = homeModel ^. #selectedWallet
          knownStakeAddresses = map (Just . view #stakeAddress) $ knownWallets ^. #stakeWallets
      in  [ Task $ do
              -- If this is a new stake address, add it to the database as well.
              if isJust stakeAddress && stakeAddress `notElem` knownStakeAddresses then do
                -- Get the new stake id for the new entry into the stake_wallet table.
                stakeWalletId <- getNextStakeWalletId databaseFile >>= fromRightOrAppError
            
                let newStakeWallet = StakeWallet
                      { network = network
                      , profileId = profileId
                      , stakeWalletId = stakeWalletId
                      , alias = alias <> "_stake"
                      , stakeAddress = fromMaybe "" stakeAddress 
                      , stakeKeyDerivation = stakeKeyDerivation
                      , registrationStatus = NotRegistered
                      , totalDelegation = 0
                      , utxoBalance = 0
                      , availableRewards = 0
                      , delegatedPool = Nothing
                      , delegatedDRep = Nothing
                      , rewardHistory = [] 
                      , linkedAddresses = []
                      }

                -- Add the new wallet to the database.
                insertStakeWallet databaseFile newStakeWallet >>= fromRightOrAppError

                return $ HomeEvent $ AddCorrespondingStakeWallet $ ProcessResults newStakeWallet
              -- Otherwise, just sync the wallets.
              else
                return $ SyncWallets $ StartProcess Nothing
          ]
    ProcessResults newStakeWallet ->
      [ Model $ model 
          & #knownWallets % #stakeWallets %~ flip snoc newStakeWallet
          & #delegationModel % #selectedWallet .~ newStakeWallet
      , Task $ return $ SyncWallets $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Change Payment Wallet Name
  -----------------------------------------------
  ChangePaymentWalletName modal -> case modal of
    -- Show the edit widget and set the extraTextField to the current alias.
    StartAdding _ -> 
      [ Model $ model 
          & #homeModel % #editingWallet .~ True
          & #homeModel % #showMorePopup .~ False
          & #homeModel % #newAliasField .~ (homeModel ^. #selectedWallet % #alias)
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info and reset the text field.
      [ Model $ model 
          & #homeModel % #editingWallet .~ False 
          & #homeModel % #newAliasField .~ ""
      ]
    ConfirmAdding ->
      -- The state is deliberately not updated in case there is an error with any of these steps.
      -- The state will be updated after everything has successfully executed.
      [ Task $ runActionOrAlert (HomeEvent . ChangePaymentWalletName . AddResult) $ do
          let currentWallet@PaymentWallet{paymentWalletId} = model ^. #homeModel % #selectedWallet
              newAlias = model ^. #homeModel % #newAliasField
              newWallet = currentWallet & #alias .~ newAlias
              -- Filter out the selected profile from the list of known payment wallets.
              otherWallets = filter (\p -> paymentWalletId /= p ^. #paymentWalletId) $
                knownWallets ^. #paymentWallets

          when (newAlias == "") $ throwIO $ AppError "New name is empty."

          when (any ((== newAlias) . view #alias) otherWallets) $ 
            throwIO $ AppError "This name is already being used by another payment wallet."

          -- Overwrite the current payment wallet name.
          insertPaymentWallet databaseFile newWallet >>= fromRightOrAppError

          return newWallet
      ] 
    AddResult newWallet@PaymentWallet{paymentWalletId} ->
      let -- Filter out the selected profile from the list of known payment wallets.
          otherWallets = filter (\p -> paymentWalletId /= p ^. #paymentWalletId) $
            knownWallets ^. #paymentWallets
          newWallets = sortOn (view #paymentWalletId) $ newWallet : otherWallets
      -- Toggle the editingWallet flag.
      in  [ Model $ model 
              & #knownWallets % #paymentWallets .~ newWallets
              & #homeModel % #editingWallet .~ False
              & #homeModel % #selectedWallet .~ newWallet
              & #homeModel % #newAliasField .~ ""
          ]

  -----------------------------------------------
  -- Delete Payment Wallet
  -----------------------------------------------
  DeletePaymentWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model 
          & #homeModel % #deletingWallet .~ True
          & #homeModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #homeModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the payment wallet from the database.
      [ Task $ runActionOrAlert (const $ HomeEvent $ DeletePaymentWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = homeModel ^. #selectedWallet % #paymentWalletId

          -- Delete the payment wallet.
          deletePaymentWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the payment wallet from the cached list of wallets.
      let currentId = homeModel ^. #selectedWallet % #paymentWalletId
          newWallets = filter (\w -> w ^. #paymentWalletId /= currentId) $
            knownWallets ^. #paymentWallets
      in  [ Model $ model 
              & #homeModel % #deletingWallet .~ False
              & #knownWallets % #paymentWallets .~ newWallets
              & #homeModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
          ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetUTxOFilters -> 
    [ Model $ model 
        & #homeModel % #utxoFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetAssetFilters -> 
    [ Model $ model 
        & #homeModel % #assetFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetHomeTxFilters -> 
    let newDefault = def & #dateRange % _1 ?~ addDays (-30) (config ^. #currentDay) in
    [ Model $ model 
        & #homeModel % #txFilterModel .~ newDefault 
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- UTxO details
  -----------------------------------------------
  ShowAllUTxODetails -> 
    [ Model $ model & #homeModel % #selectedWallet % #utxos %~ map (set #showDetails True) ]
  HideAllUTxODetails -> 
    [ Model $ model & #homeModel % #selectedWallet % #utxos %~ map (set #showDetails False) ]

  -----------------------------------------------
  -- Inspecting Transactions
  -----------------------------------------------
  InspectHomeTransaction tx -> 
    [ Model $ model & #homeModel % #inspectedTransaction ?~ tx ]
  CloseInspectedHomeTransaction -> 
    [ Model $ model & #homeModel % #inspectedTransaction .~ Nothing ]

  -----------------------------------------------
  -- Add Personal UTxO to Builder
  -----------------------------------------------
  AddSelectedUserInput (mMsg, personalUTxO) ->
    let PaymentWallet{alias,paymentAddress,paymentKeyDerivation} = homeModel ^. #selectedWallet
        newInput = personalUTxOToUserInput alias paymentAddress paymentKeyDerivation personalUTxO
     in case processNewUserInput newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Event $ Alert $ unlines $ intersperse "" $ filter (/= "")
                [ "Successfully added to builder!"
                , fromMaybe "" mMsg
                ]
            ]

  -----------------------------------------------
  -- Add Multiple Personal UTxOs to Builder
  -----------------------------------------------
  AddMultipleSelectedUserInputs (mMsg, personalUTxOs) ->
    let PaymentWallet{alias,paymentAddress,paymentKeyDerivation} = homeModel ^. #selectedWallet
        newInputs = map (personalUTxOToUserInput alias paymentAddress paymentKeyDerivation) personalUTxOs
     in case processMultipleNewUserInputs newInputs txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Event $ Alert $ unlines $ intersperse "" $ filter (/= "")
                [ "Successfully added to builder!"
                , fromMaybe "" mMsg
                ]
            ]

  -----------------------------------------------
  -- Add Collateral UTxO to Builder
  -----------------------------------------------
  AddSelectedCollateralInput personalUTxO ->
    let PaymentWallet{alias,paymentAddress,paymentKeyDerivation} = homeModel ^. #selectedWallet
        newInput = 
          personalUTxOToCollateralInput alias paymentAddress paymentKeyDerivation personalUTxO
    in  case processNewCollateralInput newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Set address as change address.
  -----------------------------------------------
  AddSelectedChangeAddress paymentAddress ->
    let oldChange = fromMaybe def $ txBuilderModel ^. #changeOutput 
        newChange = oldChange & #paymentAddress .~ paymentAddress
    in  [ Model $ model & #txBuilderModel % #changeOutput ?~ newChange
        , Task $ return $ Alert "Successfully added to builder!"
        ]

  -----------------------------------------------
  -- Inspecting Correpsonding Loan
  -----------------------------------------------
  InspectCorrespondingLoan loanId -> 
    [ Model $ model & #homeModel % #inspectedLoan ?~ loanId 
    , Event $ case Map.lookup loanId (lendingModel ^. #cachedLoanHistories) of
        Nothing -> LendingEvent $ LookupLoanHistories $ StartProcess $ Just [loanId]
        Just _ -> AppInit
    ]
  CloseInspectedCorrespondingLoan -> 
    [ Model $ model & #homeModel % #inspectedLoan .~ Nothing ]

  -----------------------------------------------
  -- Inspecting Borrower Loan
  -----------------------------------------------
  InspectBorrowerLoan loanId -> 
    [ Model $ model & #homeModel % #inspectedBorrowerLoan ?~ loanId 
    , Event $ case Map.lookup loanId (lendingModel ^. #cachedLoanHistories) of
        Nothing -> LendingEvent $ LookupLoanHistories $ StartProcess $ Just [loanId]
        Just _ -> AppInit
    ]
  CloseInspectedBorrowerLoan -> 
    [ Model $ model & #homeModel % #inspectedBorrowerLoan .~ Nothing ]

  -----------------------------------------------
  -- Add Expired Collateral claim to builder
  -----------------------------------------------
  ClaimExpiredCollateral loanUTxO ->
    let PaymentWallet{network,alias} = homeModel ^. #selectedWallet
        newInput = loanUTxOToExpiredClaim 
          network 
          alias 
          Nothing 
          Nothing 
          (config ^. #currentTime) 
          loanUTxO
        loanId = view #loanId $ fromMaybe def $ loanUTxOActiveDatum loanUTxO
     in case processNewExpiredClaim newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOnlyOneActiveBeaconAction $ newTxModel ^. #loanBuilderModel then
              [ Model $ model & #txBuilderModel .~ newTxModel
              -- Find the key input and add it to the builder.
              , Event $ HomeEvent $ AddKeyInput
                  ( Just "Also added UTxO with Key NFT to builder."
                  , mkNativeAsset Loans.activeBeaconCurrencySymbol $ loanId ^. #unLoanId
                  )
              ]
            else
              [ Event $ Alert onlyOneActiveBeaconActionError ]

  -----------------------------------------------
  -- Add Key Input
  -----------------------------------------------
  AddKeyInput (mMsg, NativeAsset{fingerprint}) ->
    let PaymentWallet{utxos} = homeModel ^. #selectedWallet
        newInput@PersonalUTxO{utxoRef} = fromMaybe def $
          find (any (\asset -> asset ^. #fingerprint == fingerprint) . view #nativeAssets) utxos
        currentInputs = map (view #utxoRef . snd) $ txBuilderModel ^. #userInputs
     in if utxoRef `elem` currentInputs 
        then [ Event $ Alert $ unlines $ intersperse "" $ filter (/= "")
                [ "Successfully added to builder!"
                , fromMaybe "" mMsg
                ]
             ]
        else [Event $ HomeEvent $ AddSelectedUserInput (mMsg, newInput)]

  -----------------------------------------------
  -- Add Multiple Key Inputs
  -----------------------------------------------
  AddMultipleKeyInputs (mMsg, keys) ->
    let PaymentWallet{utxos} = homeModel ^. #selectedWallet
        currentInputs = map (view #utxoRef . snd) $ txBuilderModel ^. #userInputs
        newInputs = filter ((`notElem` currentInputs) . view #utxoRef) 
                  $ filter (any (`elem` keys) . view #nativeAssets) utxos
     in if null newInputs
        then [ Event $ Alert $ unlines $ intersperse "" $ filter (/= "")
                [ "Successfully added to builder!"
                , fromMaybe "" mMsg
                ]
             ]
        else let newMsg = Just $ unlines 
                        $ intersperse "" 
                        $ filter (/= "") 
                        [ fromMaybe "" mMsg
                        , "Also added UTxOs with Key NFTs to builder."
                        ] 
              in [ Event $ HomeEvent $ AddMultipleSelectedUserInputs (newMsg, newInputs)]

  -----------------------------------------------
  -- Add Loan Key Burn to builder
  -----------------------------------------------
  BurnLoanKeyNFT loanId ->
    let PaymentWallet{network,alias} = homeModel ^. #selectedWallet
        newInput = loanIdToLoanKeyBurn network alias loanId
     in case processNewLoanKeyBurn newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOnlyOneActiveBeaconAction $ newTxModel ^. #loanBuilderModel then
              [ Model $ model & #txBuilderModel .~ newTxModel
              -- Find the input and add it to the builder.
              , Event $ HomeEvent $ AddKeyInput
                  ( Just "Also added UTxO with Key NFT to builder."
                  , mkNativeAsset Loans.activeBeaconCurrencySymbol $ loanId ^. #unLoanId
                  )
              ]
            else
              [ Event $ Alert onlyOneActiveBeaconActionError ]

  -----------------------------------------------
  -- Change Loan Payment Address
  -----------------------------------------------
  UpdateLenderPaymentAddress modal -> case modal of
    StartAdding mLoanUTxO -> 
      let wallet@PaymentWallet{alias,network} = homeModel ^. #selectedWallet
          newInput = 
            createNewLenderAddressUpdate network wallet alias $ fromMaybe def mLoanUTxO
       in [ Model $ model 
              & #homeModel % #newLenderAddressUpdate ?~ newInput
          ]
    CancelAdding -> 
      [ Model $ model 
          & #homeModel % #newLenderAddressUpdate .~ Nothing 
      ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (HomeEvent . UpdateLenderPaymentAddress . AddResult) $ do
          newUpdate <- fromJustOrAppError "newLenderAddressUpdate is Nothing" $ 
            homeModel ^. #newLenderAddressUpdate

          -- Verify that the loan is not already being updated.
          fromRightOrAppError $
            maybeToLeft () $ "This loan address is already being updated." <$
              find (== newUpdate ^. #loanUTxO)
                (map (view $ _2 % #loanUTxO) $ 
                    txBuilderModel ^. #loanBuilderModel % #addressUpdates)

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

          fromRightOrAppError $ updateLenderAddressDeposit verifiedUpdate minValues
      ]
    AddResult verifiedUpdate@LenderAddressUpdate{loanUTxO} ->
      let loanId = view #loanId $ fromMaybe def $ loanUTxOActiveDatum loanUTxO in
      [ Model $ model 
          & #waitingStatus % #addingToBuilder .~ False
          & #homeModel % #newLenderAddressUpdate .~ Nothing
          -- Add the new update with a dummy index.
          & #txBuilderModel % #loanBuilderModel % #addressUpdates %~ 
              flip snoc (0,verifiedUpdate)
          -- Sort the updates by the active UTxO. This is required to generate 
          -- the outputs in the proper order.
          & #txBuilderModel % #loanBuilderModel % #addressUpdates %~ 
              sortOn (view $ _2 % #loanUTxO % #utxoRef)
          -- Reindex after sorting.
          & #txBuilderModel % #loanBuilderModel % #addressUpdates %~ reIndex
          & #txBuilderModel %~ balanceTx
      , Event $ HomeEvent $ AddKeyInput
          ( Just $ unlines $ intersperse "" $ filter (/= "") 
              [ "Added UTxO with Key NFT to builder."
              , createLenderAddressDepositMsg verifiedUpdate
              ]
          , mkNativeAsset Loans.activeBeaconCurrencySymbol $ loanId ^. #unLoanId
          )
      ]

  -----------------------------------------------
  -- Inspecting Correpsonding Options Contract
  -----------------------------------------------
  InspectCorrespondingOptionsContract contractId -> 
    [ Model $ model & #homeModel % #inspectedOptionsContract ?~ contractId
    , Event $ case Map.lookup contractId (optionsModel ^. #cachedKeyContracts) of
        Nothing -> OptionsEvent $ LookupOptionsContracts $ StartProcess $ Just [contractId]
        Just _ -> AppInit
    ]
  CloseInspectedCorrespondingOptionsContract -> 
    [ Model $ model & #homeModel % #inspectedOptionsContract .~ Nothing ]

  -----------------------------------------------
  -- Add Options Key Burn to builder
  -----------------------------------------------
  BurnOptionsKeyNFT contractId ->
    let PaymentWallet{network,alias} = homeModel ^. #selectedWallet
        newInput = contractIdToOptionsKeyBurn network alias contractId
     in case processNewOptionsKeyBurn newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOnlyOneOptionsActiveBeaconAction $ newTxModel ^. #optionsBuilderModel then
              [ Model $ model & #txBuilderModel .~ newTxModel
              -- Find the input and add it to the builder.
              , Event $ HomeEvent $ AddKeyInput
                  ( Just "Also added UTxO with Key NFT to builder."
                  , mkNativeAsset Options.activeBeaconCurrencySymbol $ contractId ^. #unContractId
                  )
              ]
            else
              [ Event $ Alert onlyOneOptionsActiveBeaconActionError ]

  -----------------------------------------------
  -- Add Options Key Burn to builder
  -----------------------------------------------
  ExecuteOptionsContract optionsUTxO ->
    let PaymentWallet{network,alias} = homeModel ^. #selectedWallet
        newInput = optionsUTxOToOptionsContractExecution 
          network 
          alias 
          (config ^. #currentTime)
          optionsUTxO
        contractId = view #contractId $ fromMaybe def $ optionsUTxOActiveDatum optionsUTxO
     in case processNewOptionsExecution newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel -> 
            if hasOnlyOneOptionsActiveBeaconAction $ newTxModel ^. #optionsBuilderModel then
              [ Model $ model & #txBuilderModel .~ newTxModel
              -- Find the input and add it to the builder.
              , Event $ HomeEvent $ AddKeyInput
                  ( Just "Also added UTxO with Key NFT to builder."
                  , mkNativeAsset Options.activeBeaconCurrencySymbol $ contractId ^. #unContractId
                  )
              ]
            else
              [ Event $ Alert onlyOneOptionsActiveBeaconActionError ]

  -----------------------------------------------
  -- Events for the NFT batches
  -----------------------------------------------
  NftBatchEvent modal ->  case modal of
    AddNftToBatch nft ->
      let currentQueue = homeModel ^. #queuedNFTs
       in case maybeHead currentQueue of
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
    ClearNftBatch -> [ Model $ model & #homeModel % #queuedNFTs .~ [] ]
    ViewNftBatch -> 
      [ Model $ model & #homeModel % #viewingQueue .~ True ]
    CloseNftBatchView -> 
      [ Model $ model & #homeModel % #viewingQueue .~ False ]
    RemoveNftFromBatch nft -> 
      [ Model $ model & #homeModel % #queuedNFTs %~ filter (/= nft) 
      , Event $ Alert "Successfully removed from batch."
      ]
    ConfirmNftBatch modal' -> case modal' of
      StartAdding mNfts ->
        let nfts = fromMaybe [] mNfts
            currentPaymentWallet = homeModel ^. #selectedWallet
            firstMarketWallet@MarketWallet{network} = 
              fromMaybe def $ maybeHead $ knownWallets ^. #marketWallets
            newSale = 
              createNewSaleCreation network firstMarketWallet currentPaymentWallet nfts
         in [ Model $ model
                & #homeModel % #viewingQueue .~ False
                & #homeModel % #newSaleCreation ?~ newSale
            ]
      CancelAdding -> 
        [ Model $ model 
            & #homeModel % #viewingQueue .~ True -- return to the batch view
            & #homeModel % #newSaleCreation .~ Nothing
        ]
      ConfirmAdding ->
        [ Model $ model & #waitingStatus % #addingToBuilder .~ True
        , Task $ runActionOrAlert (HomeEvent . NftBatchEvent . ConfirmNftBatch . AddResult) $ do
            newCreation <- fromJustOrAppError "homeModel newSaleCreation is Nothing" $ 
              homeModel ^. #newSaleCreation

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
            return $ verifiedCreation & #deposit .~ minUTxOValue
        ]
      AddResult verifiedCreation ->
        -- Get the index for the new creation.
        let newIdx = length $ txBuilderModel ^. #aftermarketBuilderModel % #saleCreations 
        in  [ Model $ model 
                & #waitingStatus % #addingToBuilder .~ False
                & #txBuilderModel % #aftermarketBuilderModel % #saleCreations %~ 
                    flip snoc (newIdx,verifiedCreation)
                & #homeModel % #newSaleCreation .~ Nothing
                & #homeModel % #queuedNFTs .~ []
                & #txBuilderModel %~ balanceTx
            -- Find the required inputs and add them to the builder.
            , Event $ HomeEvent $ AddMultipleKeyInputs
                ( Just $ "This new sale requires a deposit of: " <> display (verifiedCreation ^. #deposit)
                , verifiedCreation ^. #nfts
                )
            ]

  -----------------------------------------------
  -- Inspecting Borrower Info
  -----------------------------------------------
  InspectKeyBorrowerInformation borrower@(borrowerId,_) -> 
    [ Model $ model & #homeModel % #inspectedBorrower ?~ borrower 
    , Event $ case Map.lookup borrowerId (lendingModel ^. #cachedBorrowerInfo) of
        Nothing -> LendingEvent $ LookupBorrowerInformation $ StartProcess $ Just borrower
        Just _ -> AppInit
    ]
  CloseInspectedKeyBorrowerInformation -> 
    [ Model $ model & #homeModel % #inspectedBorrower .~ Nothing ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new user input and add it to the builder. Balance the transaction after.
processNewUserInput :: UserInput -> TxBuilderModel -> Either Text TxBuilderModel
processNewUserInput u@UserInput{utxoRef} model@TxBuilderModel{userInputs, collateralInput} = do
  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This input is already being spent." <$
    find (\i -> i ^. _2 % #utxoRef == utxoRef) userInputs

  -- Verify that the new utxo is not being used as collateral.
  whenJust collateralInput $ \collateral ->
    when (collateral ^. #utxoRef == utxoRef) $ 
      Left "This input is being used as collateral."

  -- Get the input's new index.
  let newIdx = length userInputs

  -- Add the new input to the end of the list of user inputs.
  return $ balanceTx $ model & #userInputs %~ flip snoc (newIdx,u)

-- | Validate multiple new user inputs and add them to the builder. Balance the transaction after.
processMultipleNewUserInputs :: [UserInput] -> TxBuilderModel -> Either Text TxBuilderModel
processMultipleNewUserInputs newInputs model@TxBuilderModel{userInputs, collateralInput} = do
  let newRefs = map (view #utxoRef) newInputs

  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "An input is already being spent." <$
    find (\(_,i) -> (i ^. #utxoRef) `elem` newRefs) userInputs

  -- Verify that the new utxo is not being used as collateral.
  whenJust collateralInput $ \collateral ->
    when ((collateral ^. #utxoRef) `elem` newRefs) $ 
      Left "An input is being used as collateral."

  -- Get the input's new index.
  let newIdx = length userInputs
      indexedInputs = zip [newIdx..] newInputs

  -- Add the new input to the end of the list of user inputs.
  return $ balanceTx $ model & #userInputs %~ (<> indexedInputs)

-- | Validate the new collateral input and add it to the builder. Balance the transaction after.
processNewCollateralInput :: CollateralInput -> TxBuilderModel -> Either Text TxBuilderModel
processNewCollateralInput u@CollateralInput{..} model@TxBuilderModel{userInputs} = do
  -- There must be at least 5 ada in the input.
  when (lovelace < 5_000_000) $ Left "Collateral must contain at least 5 ada."

  -- There can be no native assets.
  when (nativeAssets /= []) $ Left "Collateral cannot contain native assets."

  -- Verify that the collateral utxo is not already being spent.
  maybeToLeft () $ "This input is already being spent." <$
    find (\i -> i ^. _2 % #utxoRef == utxoRef) userInputs

  -- Add the new input to the end of the list of user inputs.
  return $ balanceTx $ model & #collateralInput ?~ u

-- | Validate the new expired claim and add it to the builder. Balance the transaction after.
processNewExpiredClaim :: ExpiredClaim -> TxBuilderModel -> Either Text TxBuilderModel
processNewExpiredClaim claim model@TxBuilderModel{loanBuilderModel=LoanBuilderModel{..}} = do
  -- All actions must be for the same user.
  whenJust (claim ^. #borrowerCredential) $ \cred ->
    checkIsSameLoanUserCredential cred (model ^. #loanBuilderModel)

  -- Verify that the loan UTxO is not already being spent.
  maybeToLeft () $ "This collateral UTxO is already being claimed." <$
    find (== claim ^. #loanUTxO) (map (view $ _2 % #loanUTxO) expiredClaims)

  -- Get the input's new index.
  let newIdx = length expiredClaims

  -- Add the new close to the end of the list of claims.
  return $ balanceTx $ model 
    & #loanBuilderModel % #expiredClaims %~ flip snoc (newIdx,claim)
    & #loanBuilderModel % #userCredential %~ 
        if isJust (claim ^. #borrowerCredential) 
        then const (claim ^. #borrowerCredential)
        else id

-- | Validate the new loan key burn and add it to the builder. Balance the transaction after.
processNewLoanKeyBurn :: LoanKeyBurn -> TxBuilderModel -> Either Text TxBuilderModel
processNewLoanKeyBurn newBurn model@TxBuilderModel{loanBuilderModel=LoanBuilderModel{..}} = do
  -- Verify that the loan id is not already being burned.
  maybeToLeft () $ "This loan Key is already being burned." <$
    find (== newBurn ^. #loanIdAsset) (map (view $ _2 % #loanIdAsset) keyBurns)

  -- Get the input's new index.
  let newIdx = length keyBurns

  -- Add the new close to the end of the list of ask closes.
  return $ balanceTx $ model 
    & #loanBuilderModel % #keyBurns %~ flip snoc (newIdx,newBurn)

-- | Validate the new options key burn and add it to the builder. Balance the transaction after.
processNewOptionsKeyBurn :: OptionsKeyBurn -> TxBuilderModel -> Either Text TxBuilderModel
processNewOptionsKeyBurn newBurn model@TxBuilderModel{optionsBuilderModel=OptionsBuilderModel{..}} = do
  -- Verify that the contract id is not already being burned.
  maybeToLeft () $ "This options Key is already being burned." <$
    find (== newBurn ^. #contractIdAsset) (map (view $ _2 % #contractIdAsset) keyBurns)

  -- Get the input's new index.
  let newIdx = length keyBurns

  -- Add the new close to the end of the list of ask closes.
  return $ balanceTx $ model 
    & #optionsBuilderModel % #keyBurns %~ flip snoc (newIdx,newBurn)

-- | Validate the new options execution and add it to the builder. Balance the transaction after.
processNewOptionsExecution :: OptionsContractExecution -> TxBuilderModel -> Either Text TxBuilderModel
processNewOptionsExecution newExecution model@TxBuilderModel{optionsBuilderModel=OptionsBuilderModel{..}} = do
  -- Verify that the contract is not already being executed.
  maybeToLeft () $ "This options contract is already being executed." <$
    find (== newExecution ^. #optionsUTxO) (map (view $ _2 % #optionsUTxO) contractExecutions)

  return $ balanceTx $ model 
    -- Add the new execution with a dummy index.
    & #optionsBuilderModel % #contractExecutions %~ flip snoc (0,newExecution)
    -- Sort the executions by the UTxO. This is required to generate 
    -- the outputs in the proper order.
    & #optionsBuilderModel % #contractExecutions %~ 
        sortOn (view $ _2 % #optionsUTxO % #utxoRef)
    -- Reindex the executions after sorting.
    & #optionsBuilderModel % #contractExecutions %~ reIndex
