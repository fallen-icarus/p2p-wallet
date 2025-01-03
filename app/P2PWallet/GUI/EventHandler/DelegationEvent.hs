module P2PWallet.GUI.EventHandler.DelegationEvent
  ( 
    handleDelegationEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.Database
import P2PWallet.Actions.LookupPools
import P2PWallet.Actions.Query.Koios (runQueryDRepInformation)
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.Koios.DRep
import P2PWallet.Prelude

handleDelegationEvent :: AppModel -> DelegationEvent -> [AppEventResponse AppModel AppEvent]
handleDelegationEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeDelegationScene newScene -> 
    [ Model $ model & #delegationModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowDelegationMorePopup -> 
    [ Model $ model & #delegationModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Change the pool sort method
  -----------------------------------------------
  ChangePoolPickerSortMethod method -> 
    [ Model $ model & #delegationModel % #poolFilterModel % #sortMethod .~ method ]

  -----------------------------------------------
  -- Open the Pool Picker Widget
  -----------------------------------------------
  OpenPoolPicker ->
    [ Model $ model & #delegationModel % #showPoolPicker .~ True
    , Task $ 
        -- Only sync if no pools are cached yet.
        if null $ model ^. #delegationModel % #registeredPools
        then return $ DelegationEvent $ SyncRegisteredPools $ StartProcess Nothing
        else return AppInit
    ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairStakeWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `pairing` to `True` to display the widget for getting the new stake wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model 
          & #delegationModel % #addingWallet .~ True -- Show widget.
          & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new stake wallet info.
      [ Model $ model 
          & #delegationModel % #addingWallet .~ False
          & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & #waitingStatus % #waitingOnDevice .~ True
      , Task $ runActionOrAlert (DelegationEvent . PairStakeWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def selectedProfile
              newWallet = delegationModel ^. #newStakeWallet

          -- Get the new stake id for the new entry into the stake_wallet table.
          stakeWalletId <- getNextStakeWalletId databaseFile >>= fromRightOrAppError
          
          -- Validate the new stake wallet info, and export the required key.
          verifiedStakeWallet <- 
            pairStakeWallet network profile stakeWalletId newWallet $ knownWallets ^. #stakeWallets

          -- Add the new stake wallet to the database.
          insertStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $ model 
          & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
          & #waitingStatus % #waitingOnDevice .~ False
          & #delegationModel % #addingWallet .~ False
          & #delegationModel % #selectedWallet .~ verifiedStakeWallet
          & #scene .~ DelegationScene
       , Task $ return $ SyncWallets $ StartProcess Nothing
       ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchStakeWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `addingWallet` to `True` to display the widget for getting the new wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model 
          & #delegationModel % #addingWallet .~ True -- Show widget.
          & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new wallet info.
      [ Model $ model 
          & #delegationModel % #addingWallet .~ False
          & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    ConfirmAdding -> 
      -- Validate the information.
      [ Task $ runActionOrAlert (DelegationEvent . WatchStakeWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def selectedProfile
              newWallet = delegationModel ^. #newStakeWallet

          -- Get the new stake id for the new entry into the stake_wallet table.
          stakeWalletId <- getNextStakeWalletId databaseFile >>= fromRightOrAppError
          
          -- Validate the new wallet info.
          verifiedStakeWallet <- 
            watchStakeWallet network profile stakeWalletId newWallet $ knownWallets ^. #stakeWallets

          -- Add the new wallet to the database.
          insertStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $ model 
          & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
          & #delegationModel % #addingWallet .~ False
          & #delegationModel % #selectedWallet .~ verifiedStakeWallet
          & #scene .~ DelegationScene
       , Task $ return $ SyncWallets $ StartProcess Nothing
       ]

  -----------------------------------------------
  -- Change Stake Wallet Name
  -----------------------------------------------
  ChangeStakeWalletName modal -> case modal of
    -- Show the edit widget and set the extraTextField to the current alias.
    StartAdding _ -> 
      [ Model $ model 
          & #delegationModel % #editingWallet .~ True
          & #delegationModel % #showMorePopup .~ False
          & #delegationModel % #newAliasField .~ (delegationModel ^. #selectedWallet % #alias)
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info and reset the text field.
      [ Model $ model 
          & #delegationModel % #editingWallet .~ False 
          & #delegationModel % #newAliasField .~ ""
      ]
    ConfirmAdding ->
      -- The state is deliberately not updated in case there is an error with any of these steps.
      -- The state will be updated after everything has successfully executed.
      [ Task $ runActionOrAlert (DelegationEvent . ChangeStakeWalletName . AddResult) $ do
          let currentWallet@StakeWallet{stakeWalletId} = delegationModel ^. #selectedWallet
              newAlias = model ^. #delegationModel % #newAliasField
              newWallet = currentWallet & #alias .~ newAlias
              -- Filter out the selected wallet from the list of known wallets.
              otherWallets = filter (\p -> stakeWalletId /= p ^. #stakeWalletId) $
                knownWallets ^. #stakeWallets

          when (newAlias == "") $ throwIO $ AppError "New name is empty."

          when (any ((== newAlias) . view #alias) otherWallets) $ 
            throwIO $ AppError "This name is already being used by another stake wallet."

          -- Overwrite the current stake wallet name.
          insertStakeWallet databaseFile newWallet >>= fromRightOrAppError

          -- Overwrite the current aliases for all DeFi wallets using this stake credential.
          changeDeFiWalletAliases databaseFile stakeWalletId newAlias >>= fromRightOrAppError

          return newWallet
      ] 
    AddResult newWallet@StakeWallet{alias,stakeWalletId} ->
      [ Model $ model 
          & #knownWallets %~ updateStakeIdAliases stakeWalletId alias
          & #delegationModel % #editingWallet .~ False
          -- update all selected wallets so the new aliases take effect.
          & configureSelectedDeFiWallets
          -- override the selected wallet set by `configureSelectedDeFiWallets` for
          -- the `delegationModel`.
          & #delegationModel % #selectedWallet .~ newWallet
          & #delegationModel % #newAliasField .~ ""
      ]

  -----------------------------------------------
  -- Delete Stake Wallet
  -----------------------------------------------
  DeleteStakeWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model 
          & #delegationModel % #deletingWallet .~ True
          & #delegationModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #delegationModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ DelegationEvent $ DeleteStakeWallet PostDeletionAction) $ do
          -- Get the stake id for the stake wallet to delete.
          let currentId = delegationModel ^. #selectedWallet % #stakeWalletId

          -- Delete the stake wallet.
          deleteStakeWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the wallet from the cache.
      let currentId = delegationModel ^. #selectedWallet % #stakeWalletId
      in [ Model $ model 
            & #delegationModel % #deletingWallet .~ False
            & #knownWallets %~ deleteStakeIdWallets currentId
            & configureSelectedDeFiWallets
         ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetPoolFilters -> 
    [ Model $ model 
        & #delegationModel % #poolFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Syncing Registered Pools
  -----------------------------------------------
  SyncRegisteredPools modal -> case modal of
    StartProcess _ -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & #waitingStatus % #syncingPools .~ True 
      , Task $ do
          runActionOrAlert (DelegationEvent . SyncRegisteredPools . ProcessResults) $ 
            lookupRegisteredPools (config ^. #network)
      ]
    ProcessResults resp -> 
      -- Disable `syncing` and update the list of pools. 
      [ Model $ model 
          & #waitingStatus % #syncingPools .~ False
          & #delegationModel % #registeredPools .~ resp
      ]

  -----------------------------------------------
  -- Add User Certificate to Builder
  -----------------------------------------------
  AddSelectedUserCertificate (mNameAndTicker,certificateAction) ->
    let StakeWallet{alias,stakeAddress,stakeKeyDerivation} = delegationModel ^. #selectedWallet 
        userCertificate = UserCertificate
          { stakeAddress = stakeAddress
          , stakeKeyDerivation = stakeKeyDerivation
          , certificateAction = certificateAction
          , walletAlias = alias
          , poolName = mNameAndTicker
          }
    in case processNewUserCertificate userCertificate txBuilderModel of
        Left err -> [ Task $ return $ Alert err ]
        Right newTxModel ->
          [ Model $ model & #txBuilderModel .~ newTxModel
          , Task $ return $ Alert "Successfully added to builder!"
          ]

  -----------------------------------------------
  -- Add User Withdrawal to Builder
  -----------------------------------------------
  AddSelectedUserWithdrawal StakeWallet{alias,stakeAddress,stakeKeyDerivation,availableRewards} ->
    let userWithdrawal = UserWithdrawal
          { stakeAddress = stakeAddress
          , stakeKeyDerivation = stakeKeyDerivation
          , lovelace = availableRewards
          , walletAlias = alias
          }
        filteredWithdrawals = flip filter (txBuilderModel ^. #userWithdrawals) $ \(_,wtdr) ->
            wtdr ^. #stakeAddress /= stakeAddress
    in  [ Model $ model 
            & #txBuilderModel % #userWithdrawals .~ 
                -- Add the new withdrawal, sort them by stake address, and immediately reindex.
                reIndex (sortOn snd $ (0,userWithdrawal) : filteredWithdrawals)
            & #txBuilderModel %~ balanceTx
        , Task $ return $ Alert "Successfully added to builder!"
        ]

  -----------------------------------------------
  -- Add new drep delegation
  -----------------------------------------------
  AddDrepDelegation modal -> case modal of
    StartAdding _ -> 
      [ Model $ model 
          & #delegationModel % #newDrepDelegation ?~ AlwaysAbstainDelegation
          & #delegationModel % #newDrepId .~ ""
      ]
    CancelAdding -> 
      [ Model $ model 
          & #delegationModel % #newDrepDelegation .~ Nothing
          & #delegationModel % #newDrepId .~ ""
      ]
    ConfirmAdding -> case delegationModel ^? #newDrepDelegation % _Just of
      Nothing -> [Event $ Alert "newDrepDelegation is Nothing"]
      Just (DRepDelegation _ _) ->
        [ Model $ model & #waitingStatus % #syncingDRepInfo .~ True
        , Task $ runActionOrAlert (DelegationEvent . AddDrepDelegation . AddResult) $ do
            let network = config ^. #network
                targetDrepId = DRepID $ delegationModel ^. #newDrepId

            -- Verify the DRepID is a real DRep.
            DRep{isScript} <- runQueryDRepInformation network targetDrepId >>= fromRightOrAppError

            -- Replace the dummy value in `DRepDelegation` and return it. 
            return $ DRepDelegation targetDrepId isScript
        ]
      Just voteDeleg -> [Event $ DelegationEvent $ AddDrepDelegation $ AddResult voteDeleg]
    AddResult voteDeleg -> 
      [ Model $ model 
          & #waitingStatus % #syncingDRepInfo .~ False
          & #delegationModel % #newDrepDelegation .~ Nothing
      , Event $ DelegationEvent $ AddSelectedUserCertificate (Nothing, VoteDelegation voteDeleg)
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Validate the new user certificate and add it to the builder. Balance the transaction after.
processNewUserCertificate :: UserCertificate -> TxBuilderModel -> Either Text TxBuilderModel
processNewUserCertificate u@UserCertificate{..} model@TxBuilderModel{userCertificates} = do
    -- Make sure this is not a duplicate registration.
    when (certificateAction == Registration) $
      flip when (Left "This stake address is already being registered.") $
        flip any userCertificates $ \(_,userCert) -> and
          [ isJust (userCert ^? #certificateAction % _Registration)
          , userCert ^. #stakeAddress == stakeAddress
          ]
      
    -- Make sure this is not a duplicate deregistration.
    when (certificateAction == Deregistration) $
      flip when (Left "This stake address is already being deregistered.") $
        flip any userCertificates $ \(_,userCert) -> and
          [ isJust (userCert ^? #certificateAction % _Deregistration)
          , userCert ^. #stakeAddress == stakeAddress
          ]

    return $ balanceTx $ model 
      -- Add the new certificate to the list, sort the list so that stake address actions are
      -- grouped together, and then immediately re-index the list.
      & #userCertificates .~ reIndex (sortOn snd $ (0,u) : filteredCertificates)
  where
    -- Previous delegation certificates for this stake address must be replaced by the new one.
    -- If a deregistration cert was entered first, it should be overridden by a delegation cert,
    -- and vice versa.
    filteredCertificates :: [(Int,UserCertificate)]
    filteredCertificates = case certificateAction of
      Registration -> 
        -- Don't filter the certificates. Duplicate registrations would have been caught already.
        userCertificates
      Deregistration -> flip filter userCertificates $ \(_,userCert) -> 
        -- All previous certificates for this stake address should be overridden.
        userCert ^. #stakeAddress /= stakeAddress
      StakeDelegation _ -> flip filter userCertificates $ \(_,userCert) -> 
        -- All previous deregistration certificates and delegation certificates for this stake 
        -- address should be overridden.
        not $ and
          [ userCert ^. #stakeAddress == stakeAddress
          , isJust (userCert ^? #certificateAction % _StakeDelegation) ||
              isJust (userCert ^? #certificateAction % _Deregistration)
          ]
      VoteDelegation _ -> flip filter userCertificates $ \(_,userCert) -> 
        -- All previous deregistration certificates and vote certificates for this stake 
        -- address should be overridden.
        not $ and
          [ userCert ^. #stakeAddress == stakeAddress
          , isJust (userCert ^? #certificateAction % _VoteDelegation) ||
              isJust (userCert ^? #certificateAction % _Deregistration)
          ]
