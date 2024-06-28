module P2PWallet.GUI.EventHandler.DelegationEvent
  ( 
    handleDelegationEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.Database
import P2PWallet.Actions.LookupPools
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleDelegationEvent :: AppModel -> DelegationEvent -> [AppEventResponse AppModel AppEvent]
handleDelegationEvent model@AppModel{..} evt = case evt of
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
        then return $ DelegationEvent $ SyncRegisteredPools StartProcess
        else return AppInit
    ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairStakeWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `pairing` to `True` to display the widget for getting the new stake wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model & #delegationModel % #addingWallet .~ True -- Show widget.
                      & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new stake wallet info.
      [ Model $ model & #delegationModel % #addingWallet .~ False
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
          stakeId <- getNextStakeId databaseFile >>= fromRightOrAppError
          
          -- Validate the new stake wallet info, and export the required key.
          verifiedStakeWallet <- pairStakeWallet network profile stakeId newWallet

          -- Add the new stake wallet to the database.
          insertStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $
          model & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
                & #waitingStatus % #waitingOnDevice .~ False
                & #delegationModel % #addingWallet .~ False
                & #delegationModel % #selectedWallet .~ verifiedStakeWallet
                & #scene .~ DelegationScene
       , Task $ return $ SyncWallets StartProcess
       ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchStakeWallet modal -> case modal of
    StartAdding _ -> 
      -- Set `addingWallet` to `True` to display the widget for getting the new wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model & #delegationModel % #addingWallet .~ True -- Show widget.
                      & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new wallet info.
      [ Model $ model & #delegationModel % #addingWallet .~ False
                      & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    ConfirmAdding -> 
      -- Validate the information.
      [ Task $ runActionOrAlert (DelegationEvent . WatchStakeWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def selectedProfile
              newWallet = delegationModel ^. #newStakeWallet

          -- Get the new stake id for the new entry into the stake_wallet table.
          stakeId <- getNextStakeId databaseFile >>= fromRightOrAppError
          
          -- Validate the new wallet info.
          verifiedStakeWallet <- watchStakeWallet network profile stakeId newWallet

          -- Add the new wallet to the database.
          insertStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $
          model & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
                & #delegationModel % #addingWallet .~ False
                & #delegationModel % #selectedWallet .~ verifiedStakeWallet
                & #scene .~ DelegationScene
       , Task $ return $ SyncWallets StartProcess
       ]

  -----------------------------------------------
  -- Change Stake Wallet Name
  -----------------------------------------------
  ChangeStakeWalletName modal -> case modal of
    -- Show the edit widget and set the extraTextField to the current alias.
    StartAdding _ -> 
      [ Model $ model & #delegationModel % #editingWallet .~ True
                      & #delegationModel % #showMorePopup .~ False
                      & #extraTextField .~ (delegationModel ^. #selectedWallet % #alias)
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info and reset the text field.
      [ Model $ model & #delegationModel % #editingWallet .~ False 
                      & #extraTextField .~ ""
      ]
    ConfirmAdding ->
      -- The state is deliberately not updated in case there is an error with any of these steps.
      -- The state will be updated after everything has successfully executed.
      [ Task $ runActionOrAlert (DelegationEvent . ChangeStakeWalletName . AddResult) $ do
          let currentWallet = delegationModel ^. #selectedWallet
              newAlias = model ^. #extraTextField
              newWallet = currentWallet & #alias .~ newAlias

          when (newAlias == "") $ throwIO $ AppError "New name is empty."

          -- Overwrite the current stake wallet name.
          insertStakeWallet databaseFile newWallet >>= fromRightOrAppError

          return newWallet
      ] 
    AddResult newWallet@StakeWallet{stakeId} ->
      let -- Filter out the selected wallet from the list of known wallets.
          otherWallets = filter (\p -> stakeId /= p ^. #stakeId) $
            knownWallets ^. #stakeWallets
          newWallets = sortOn (view #stakeId) $ newWallet : otherWallets
      in [ Model $ 
             model & #knownWallets % #stakeWallets .~ newWallets
                   & #delegationModel % #editingWallet .~ False
                   & #delegationModel % #selectedWallet .~ newWallet
                   & #extraTextField .~ ""
         ]

  -----------------------------------------------
  -- Delete Stake Wallet
  -----------------------------------------------
  DeleteStakeWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #delegationModel % #deletingWallet .~ True
                      & #delegationModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #delegationModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ DelegationEvent $ DeleteStakeWallet PostDeletionAction) $ do
          -- Get the stake id for the stake wallet to delete.
          let currentId = delegationModel ^. #selectedWallet % #stakeId

          -- Delete the stake wallet.
          deleteStakeWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the wallet from the cache.
      let currentId = delegationModel ^. #selectedWallet % #stakeId
          newWallets = filter (\w -> w ^. #stakeId /= currentId) $ knownWallets ^. #stakeWallets
      in [ Model $ model & #delegationModel % #deletingWallet .~ False
                         & #knownWallets % #stakeWallets .~ newWallets
                         & #delegationModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
         ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetPoolFilters -> 
    [ Model $ model & #delegationModel % #poolFilterModel .~ def
                    & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Syncing Registered Pools
  -----------------------------------------------
  SyncRegisteredPools modal -> case modal of
    StartProcess -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & #waitingStatus % #syncingPools .~ True 
      , Task $ do
          runActionOrAlert (DelegationEvent . SyncRegisteredPools . ProcessResults) $ 
            lookupRegisteredPools (config ^. #network)
      ]
    ProcessResults resp -> 
      -- Disable `syncing` and update the list of pools. 
      [ Model $ 
          model & #waitingStatus % #syncingPools .~ False
                & #delegationModel % #registeredPools .~ resp
      ]

  -----------------------------------------------
  -- Add User Certificate to Builder
  -----------------------------------------------
  AddSelectedUserCertificate (mNameAndTicker,certificateAction) ->
    let StakeWallet{alias,stakeAddress,stakeKeyPath} = delegationModel ^. #selectedWallet 
        userCertificate = UserCertificate
          { stakeAddress = stakeAddress
          , stakeKeyPath = stakeKeyPath
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
  AddSelectedUserWithdrawal StakeWallet{alias,stakeAddress,stakeKeyPath,availableRewards} ->
    let userWithdrawal = UserWithdrawal
          { stakeAddress = stakeAddress
          , stakeKeyPath = stakeKeyPath
          , lovelace = availableRewards
          , walletAlias = alias
          }
        filteredWithdrawals = flip filter (txBuilderModel ^. #userWithdrawals) $ \(_,wtdr) ->
            wtdr ^. #stakeAddress /= stakeAddress
    in [ Model $ model 
            & #txBuilderModel % #userWithdrawals .~ 
                -- Add the new withdrawal, sort them by stake address, and immediately reindex.
                reIndex (sortOn snd $ (0,userWithdrawal) : filteredWithdrawals)
            & #txBuilderModel %~ balanceTx
        , Task $ return $ Alert "Successfully added to builder!"
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

    return $ balanceTx $ 
      model 
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
      Delegation _ -> flip filter userCertificates $ \(_,userCert) -> 
        -- All previous deregistration certificates and delegation certificates for this stake 
        -- address should be overridden.
        not $ and
          [ userCert ^. #stakeAddress == stakeAddress
          , isJust (userCert ^? #certificateAction % _Delegation) ||
              isJust (userCert ^? #certificateAction % _Deregistration)
          ]
