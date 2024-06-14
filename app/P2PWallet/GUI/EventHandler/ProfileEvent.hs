{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.ProfileEvent
  ( 
    handleProfileEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Profile
import P2PWallet.Data.TickerMap
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

handleProfileEvent :: AppModel -> ProfileEvent -> [AppEventResponse AppModel AppEvent]
handleProfileEvent model@AppModel{..} evt = case evt of
  -- After loading the profiles from the database, update the model.
  LoadKnownProfiles profiles -> 
    -- When the profiles are loaded, move the user to the profile picker scene.
    [ Model $ model & #scene .~ ProfilesScene 
                    & #knownProfiles .~ profiles
    ]

  -- Set the desired profile to selected and get the associated wallets from the database.
  LoadSelectedProfile profile -> 
    [ Model $ model & #selectedProfile .~ Just profile 
                    & #loadingProfile .~ True
    , Task $ runActionOrAlert (ProfileEvent . LoadProfileInfo) $
        (,,) <$> (loadWallets databaseFile profile >>= fromRightOrAppError)
             <*> (loadAddressBook databaseFile profile >>= fromRightOrAppError)
             <*> (loadTickerInfo databaseFile >>= fromRightOrAppError)
    ]

  -- After loading the profile info from the database, update the model.
  LoadProfileInfo (wallets@Wallets{..}, contacts, tickers) -> 
    [ Model $ model & #knownWallets .~ wallets
                    & #scene .~ HomeScene
                    & #loadingProfile .~ False
                    & #homeModel % #selectedWallet .~ fromMaybe def (maybeHead paymentWallets)
                    & #delegationModel % #selectedWallet .~ fromMaybe def (maybeHead stakeWallets)
                    & #addressBook .~ contacts
                    & #tickerMap .~ toTickerMap tickers
                    & #reverseTickerMap .~ toReverseTickerMap tickers
                    & #fingerprintMap .~ genFingerprintMap wallets
    -- , Task $ return $ SyncWallets StartSync
    ]

  LogoutProfile -> 
    -- Log the user out of the currently selected profile, and return the user to the profile 
    -- picker screen.
    [ Model $ model & #scene .~ ProfilesScene 
                    & #selectedProfile .~ Nothing
                    & #knownWallets .~ def
                    & #addressBook .~ []
    ]

  -----------------------------------------------
  -- Adding new profiles
  -----------------------------------------------
  AddNewProfile modal -> case modal of
    -- Show the addNewProfile widget and clear the old information.
    StartAdding _ -> 
      [ Model $ model & #newProfile .~ def
                      & #addingProfile .~ True
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info.
      [ Model $ model & #addingProfile .~ False ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (ProfileEvent . AddNewProfile . AddResult) $ do
          -- Get the next row id for the profiles table.
          newProfileId <- getNextProfileId databaseFile >>= fromRightOrAppError

          -- Verify the user supplied information.
          verifiedProfile <- fromRightOrAppError $
            toProfile (config ^. #network) newProfileId newProfile knownProfiles

          -- Add the new profile.
          addNewProfile databaseFile verifiedProfile >>= fromRightOrAppError

          return verifiedProfile
      ]
    AddResult verifiedProfile ->
      -- Take the user to the home page. Also toggle the addingProfile flag.
      [ Model $ 
          model & #knownProfiles %~ flip snoc verifiedProfile
                & #addingProfile .~ False
      , Task $ return $ ProfileEvent $ LoadSelectedProfile verifiedProfile
      ]

  -----------------------------------------------
  -- Change Profile Name
  -----------------------------------------------
  ChangeProfileName modal -> case modal of
    -- Show the edit widget and set the newProfile information to the current profile.
    StartAdding _ -> 
      [ Model $ model & #extraTextField .~ fromMaybe "" (selectedProfile ^? _Just % #alias)
                      & #addingProfile .~ True
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info.
      [ Model $ model & #addingProfile .~ False
                      & #extraTextField .~ ""
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (ProfileEvent . ChangeProfileName . AddResult) $ do
          let currentProfile = fromMaybe def selectedProfile
              -- Get the row id for the profile being updated.
              currentId = currentProfile ^. #profileId
              -- Filter out the selected profile from the list of known profiles.
              otherProfiles = filter (\p -> currentId /= p ^. #profileId) knownProfiles
              newAlias = model ^. #extraTextField
              newProfile' = currentProfile & #alias .~ newAlias

          -- Check if the alias name is already being used.
          when (newAlias == "") $ 
            throwIO $ AppError "New name is empty."
          when (any (\p -> p ^. #alias == newAlias) otherProfiles) $ 
            throwIO $ AppError "Name is already being used."

          -- Overwrite the current profile name.
          addNewProfile databaseFile newProfile' >>= fromRightOrAppError

          return newAlias
      ] 
    AddResult newAlias ->
      let currentProfile = fromMaybe def selectedProfile
          -- Get the row id for the profile being updated.
          currentId = currentProfile ^. #profileId
          -- Filter out the selected profile from the list of known profiles.
          otherProfiles = filter (\p -> currentId /= p ^. #profileId) knownProfiles
          newProfile' = currentProfile & #alias .~ newAlias
          newProfiles = sortOn (view #profileId) $ newProfile' : otherProfiles
      -- Toggle the addingProfile flag.
      in [ Model $ 
             model & #knownProfiles .~ newProfiles
                   & #addingProfile .~ False
                   & #selectedProfile .~ Just newProfile'
         ]

  -----------------------------------------------
  -- Delete Profile
  -----------------------------------------------
  DeleteProfile modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #deletingProfile .~ True
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #deletingProfile .~ False ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ ProfileEvent $ DeleteProfile PostDeletionAction) $ do
          -- Get the row id for the profile to delete.
          let currentId = fromMaybe 0 $ model ^? #selectedProfile % _Just % #profileId

          -- Delete the profile.
          deleteProfile databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Toggle the deletingProfile flag.
      [ Model $ model & #deletingProfile .~ False
                      & #selectedProfile .~ Nothing
      , Task $ return $ SetNetwork $ config ^. #network
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Create the fingerprint map.
genFingerprintMap :: Wallets -> Map Text (Text,Text)
genFingerprintMap Wallets{paymentWallets} =
  let nativeAssets = concatMap (view #nativeAssets) paymentWallets
  in fromList $ map (\NativeAsset{..} -> (fingerprint,(policyId,tokenName))) nativeAssets
