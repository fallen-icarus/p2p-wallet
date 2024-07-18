module P2PWallet.GUI.EventHandler.ProfileEvent
  ( 
    handleProfileEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

handleProfileEvent :: AppModel -> ProfileEvent -> [AppEventResponse AppModel AppEvent]
handleProfileEvent model@AppModel{..} evt = case evt of
  -- Load all tracked profiles for the specified network.
  LoadKnownProfiles modal -> case modal of
    StartProcess -> 
      [ Task $ runActionOrAlert (ProfileEvent . LoadKnownProfiles . ProcessResults) $
          -- Try to load the profiles from the sqlite database. Show the user any error that appears.
          loadProfiles databaseFile (config ^. #network) >>= fromRightOrAppError
      ]
    ProcessResults profiles ->
      -- When the profiles are loaded, move the user to the profile picker scene.
      [ Model $ model 
          & #scene .~ ProfilesScene 
          & #profileModel % #knownProfiles .~ profiles
      ]

  -- Set the desired profile to selected.
  LoadSelectedProfile profile -> 
    [ Model $ model 
        & #selectedProfile ?~ profile 
        & #waitingStatus % #loadingProfile .~ True
    , Task $ return $ ProfileEvent $ LoadProfileInfo StartProcess
    ]

  -- Load all info for the selected profile.
  LoadProfileInfo modal -> case modal of
    StartProcess ->
      let profile = fromMaybe def selectedProfile in
      [ Task $ runActionOrAlert (ProfileEvent . LoadProfileInfo . ProcessResults) $
          (,,) <$> (loadWallets databaseFile profile >>= fromRightOrAppError)
               <*> (loadAddressBook databaseFile profile >>= fromRightOrAppError)
               <*> (loadTickerInfo databaseFile >>= fromRightOrAppError)
      ]
    ProcessResults (wallets@Wallets{..}, contacts, tickers) -> 
      [ Model $ model 
          & #knownWallets .~ wallets
          & #scene .~ HomeScene
          & #notifications .~ []
          & #waitingStatus % #loadingProfile .~ False
          & #homeModel % #selectedWallet .~ fromMaybe def (maybeHead paymentWallets)
          -- Set the selected stake and DeFi wallets.
          & configureSelectedDeFiWallets
          & #addressBook .~ contacts
          & #tickerMap .~ toTickerMap tickers
          & #reverseTickerMap .~ toReverseTickerMap tickers
          & #fingerprintMap .~ 
              toFingerprintMap (concatMap (view #nativeAssets) paymentWallets)
      -- , Task $ return $ SyncWallets StartSync
      ]

  -- Log the user out of the currently selected profile, and return the user to the profile 
  -- picker screen.
  LogoutProfile -> 
    [ Model $ model 
        & #scene .~ ProfilesScene 
        & #selectedProfile .~ Nothing
        & #knownWallets .~ def
        & #notifications .~ []
        & #addressBook .~ []
        & #txBuilderModel .~ def -- reset builder
    ]

  -----------------------------------------------
  -- Adding new profiles
  -----------------------------------------------
  AddNewProfile modal -> case modal of
    -- Show the addNewProfile widget and clear the old information.
    StartAdding _ -> 
      [ Model $ model 
          & #profileModel % #newProfile .~ def
          & #profileModel % #addingProfile .~ True
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info.
      [ Model $ model & #profileModel % #addingProfile .~ False ]
    ConfirmAdding ->
      let ProfileModel{..} = profileModel in
      [ Task $ runActionOrAlert (ProfileEvent . AddNewProfile . AddResult) $ do
          -- Get the next row id for the profiles table.
          newProfileId <- getNextProfileId databaseFile >>= fromRightOrAppError

          -- Verify the user supplied information.
          verifiedProfile <- fromRightOrAppError $
            verifyNewProfile (config ^. #network) newProfileId newProfile knownProfiles

          -- Add the new profile.
          insertProfile databaseFile verifiedProfile >>= fromRightOrAppError

          return verifiedProfile
      ]
    AddResult verifiedProfile ->
      [ Model $ model 
          & #profileModel % #knownProfiles %~ flip snoc verifiedProfile
          & #profileModel % #addingProfile .~ False
      , Task $ return $ ProfileEvent $ LoadSelectedProfile verifiedProfile
      ]

  -----------------------------------------------
  -- Change Profile Name
  -----------------------------------------------
  ChangeProfileName modal -> case modal of
    -- Show the edit widget and set the newProfile information to the current profile.
    StartAdding _ -> 
      [ Model $ model 
          & #profileModel % #newProfile .~ maybe def toNewProfile selectedProfile
          & #profileModel % #addingProfile .~ True
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info.
      [ Model $ model 
          & #profileModel % #addingProfile .~ False
          & #profileModel % #newProfile .~ def
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (ProfileEvent . ChangeProfileName . AddResult) $ do
          let Profile{network,profileId} = fromMaybe def selectedProfile
              ProfileModel{newProfile,knownProfiles} = profileModel
              -- Filter out the selected profile from the list of known profiles.
              otherProfiles = filter (\p -> profileId /= p ^. #profileId) knownProfiles

          -- Verify the user supplied information.
          verifiedProfile <- fromRightOrAppError $
            verifyNewProfile network profileId newProfile otherProfiles

          -- Overwrite the current profile info.
          insertProfile databaseFile verifiedProfile >>= fromRightOrAppError

          return verifiedProfile
      ] 
    AddResult verifiedProfile@Profile{profileId} ->
      let knownProfiles = profileModel ^. #knownProfiles
          otherProfiles = filter (\p -> profileId /= p ^. #profileId) knownProfiles
          newProfiles = sortOn (view #profileId) $ verifiedProfile : otherProfiles
      in  [ Model $ model 
              & #profileModel % #knownProfiles .~ newProfiles
              & #profileModel % #addingProfile .~ False
              & #profileModel % #newProfile .~ def
              & #selectedProfile ?~ verifiedProfile
          ]

  -----------------------------------------------
  -- Delete Profile
  -----------------------------------------------
  DeleteProfile modal -> case modal of
    GetDeleteConfirmation _ -> 
      [ Model $ model & #profileModel % #deletingProfile .~ True ]
    CancelDeletion -> 
      [ Model $ model & #profileModel % #deletingProfile .~ False ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ ProfileEvent $ DeleteProfile PostDeletionAction) $ do
          -- Get the row id for the profile to delete.
          let currentId = fromMaybe 0 $ model ^? #selectedProfile % _Just % #profileId

          -- Delete the profile.
          deleteProfile databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      [ Model $ model 
          & #profileModel % #deletingProfile .~ False
          & #selectedProfile .~ Nothing
      , Task $ return $ ProfileEvent $ LoadKnownProfiles StartProcess
      ]
