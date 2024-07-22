module P2PWallet.GUI.EventHandler.HomeEvent
  ( 
    handleHomeEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
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
          paymentId <- getNextPaymentIdAcrossTables databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and export the required keys.
          verifiedPaymentWallet <- 
            pairPaymentWallet network profile paymentId newWallet $ knownWallets ^. #paymentWallets

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
      , Task $ return $ HomeEvent $ AddCorrespondingStakeWallet StartProcess
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
          paymentId <- getNextPaymentIdAcrossTables databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and extract the stake address, if any.
          verifiedPaymentWallet <- 
            watchPaymentWallet network profile paymentId newWallet $ knownWallets ^. #paymentWallets

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
      , Task $ return $ HomeEvent $ AddCorrespondingStakeWallet StartProcess
      ]

  -----------------------------------------------
  -- Add the corresponding stake wallet when adding a payment wallet
  -----------------------------------------------
  AddCorrespondingStakeWallet modal -> case modal of
    StartProcess ->
      let PaymentWallet{..} = homeModel ^. #selectedWallet
          knownStakeAddresses = map (Just . view #stakeAddress) $ knownWallets ^. #stakeWallets
      in  [ Task $ do
              -- If this is a new stake address, add it to the database as well.
              if isJust stakeAddress && stakeAddress `notElem` knownStakeAddresses then do
                -- Get the new stake id for the new entry into the stake_wallet table.
                stakeId <- getNextStakeId databaseFile >>= fromRightOrAppError
            
                let newStakeWallet = StakeWallet
                      { network = network
                      , profileId = profileId
                      , stakeId = stakeId
                      , alias = alias <> "_stake"
                      , stakeAddress = fromMaybe "" stakeAddress 
                      , stakeKeyDerivation = stakeKeyDerivation
                      , registrationStatus = NotRegistered
                      , totalDelegation = 0
                      , utxoBalance = 0
                      , availableRewards = 0
                      , delegatedPool = Nothing
                      , rewardHistory = [] 
                      , linkedAddresses = []
                      }

                -- Add the new wallet to the database.
                insertStakeWallet databaseFile newStakeWallet >>= fromRightOrAppError

                return $ HomeEvent $ AddCorrespondingStakeWallet $ ProcessResults newStakeWallet
              -- Otherwise, just sync the wallets.
              else
                return $ SyncWallets StartProcess
          ]
    ProcessResults newStakeWallet ->
      [ Model $ model 
          & #knownWallets % #stakeWallets %~ flip snoc newStakeWallet
          & #delegationModel % #selectedWallet .~ newStakeWallet
      , Task $ return $ SyncWallets StartProcess
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
          let currentWallet@PaymentWallet{paymentId} = model ^. #homeModel % #selectedWallet
              newAlias = model ^. #homeModel ^. #newAliasField
              newWallet = currentWallet & #alias .~ newAlias
              -- Filter out the selected profile from the list of known payment wallets.
              otherWallets = filter (\p -> paymentId /= p ^. #paymentId) $
                knownWallets ^. #paymentWallets

          when (newAlias == "") $ throwIO $ AppError "New name is empty."

          when (any ((== newAlias) . view #alias) otherWallets) $ 
            throwIO $ AppError "This name is already being used by another payment wallet."

          -- Overwrite the current payment wallet name.
          insertPaymentWallet databaseFile newWallet >>= fromRightOrAppError

          return newWallet
      ] 
    AddResult newWallet@PaymentWallet{paymentId} ->
      let -- Filter out the selected profile from the list of known payment wallets.
          otherWallets = filter (\p -> paymentId /= p ^. #paymentId) $
            knownWallets ^. #paymentWallets
          newWallets = sortOn (view #paymentId) $ newWallet : otherWallets
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
          let currentId = homeModel ^. #selectedWallet % #paymentId

          -- Delete the payment wallet.
          deletePaymentWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the payment wallet from the cached list of wallets.
      let currentId = homeModel ^. #selectedWallet % #paymentId
          newWallets = filter (\w -> w ^. #paymentId /= currentId) $
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
  AddSelectedUserInput personalUTxO ->
    let PaymentWallet{alias,paymentAddress,paymentKeyDerivation} = homeModel ^. #selectedWallet
        newInput = personalUTxOToUserInput alias paymentAddress paymentKeyDerivation personalUTxO
    in  case processNewUserInput newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
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
