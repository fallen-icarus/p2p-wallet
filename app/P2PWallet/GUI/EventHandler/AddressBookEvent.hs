{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.AddressBookEvent
  ( 
    handleAddressBookEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AddressBook
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Profile
import P2PWallet.Prelude

handleAddressBookEvent :: AppModel -> AddressBookEvent -> [AppEventResponse AppModel AppEvent]
handleAddressBookEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Add new contact
  -----------------------------------------------
  AddNewAddressEntry modal -> case modal of
    StartAdding _ -> 
      [ Model $ model & #addressBookModel % #addingContact .~ True -- Show widget.
      ]
    CancelAdding -> 
      [ Model $ model & #addressBookModel % #addingContact .~ False
                      & #addressBookModel % #newAddressEntry .~ def -- Clear for next time.
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AddressBookEvent . AddNewAddressEntry . AddResult) $ do
          let Profile{profileId} = fromMaybe def selectedProfile
              newEntry = addressBookModel ^. #newAddressEntry
              network = config ^. #network

          -- Get the new address entry id for the new entry into the address_book table.
          entryId <- getNextAddressEntryId databaseFile >>= fromRightOrAppError
          
          -- Validate the new contact info.
          newVerifiedEntry <- fromRightOrAppError $
            processNewAddressEntry newEntry network profileId entryId addressBook

          -- Add the new entry to the database.
          addNewAddressEntry databaseFile [newVerifiedEntry] >>= fromRightOrAppError

          return newVerifiedEntry
      ]
    AddResult verifiedEntry ->
      [ Model $
          model & #addressBook %~ sortOn (view #alias) . flip snoc verifiedEntry
                & #addressBookModel % #addingContact .~ False
                & #addressBookModel % #newAddressEntry .~ def -- Clear information.
      ]

  -----------------------------------------------
  -- Edit the contact
  -----------------------------------------------
  ChangeAddressEntry modal -> case modal of
    StartAdding mTarget -> 
      let newEntry = 
            def & #alias .~ fromMaybe "" (mTarget ^? _Just % #alias)
                & #paymentAddress .~ maybe "" toText (mTarget ^? _Just % #paymentAddress)
      in  [ Model $ model & #addressBookModel % #editingContact .~ True -- Show widget.
                          & #addressBookModel % #selectedContact .~ mTarget
                          & #addressBookModel % #newAddressEntry .~ newEntry
          ]
    CancelAdding -> 
      [ Model $ model & #addressBookModel % #editingContact .~ False 
                      & #addressBookModel % #newAddressEntry .~ def -- Clear for next time.
                      & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AddressBookEvent . ChangeAddressEntry . AddResult) $ do
          let Profile{profileId} = fromMaybe def selectedProfile
              newEntry = addressBookModel ^. #newAddressEntry
              network = config ^. #network
              targetId = fromMaybe 0 $ 
                addressBookModel ^? #selectedContact % _Just % #addressEntryId
              restOfBook = filter (\b -> b ^. #addressEntryId /= targetId) addressBook

          -- Validate the new contact info.
          newVerifiedEntry <- fromRightOrAppError $
            processNewAddressEntry newEntry network profileId targetId restOfBook

          -- Add the new entry to the database.
          addNewAddressEntry databaseFile [newVerifiedEntry] >>= fromRightOrAppError

          return newVerifiedEntry
      ]
    AddResult AddressEntry{alias,paymentAddress} ->
      let target@AddressEntry{addressEntryId} = 
            fromMaybe def $ addressBookModel ^. #selectedContact
          restOfBook = filter (\b -> b ^. #addressEntryId /= addressEntryId) addressBook
          newInfo = target & #alias .~ alias
                           & #paymentAddress .~ paymentAddress
          newBook = sortOn (view #alias) $ newInfo : restOfBook
      in  [ Model $
              model & #addressBook .~ newBook
                    & #addressBookModel % #editingContact .~ False
                    & #addressBookModel % #newAddressEntry .~ def -- Clear information.
                    & #addressBookModel % #selectedContact .~ Nothing
          ]

  -----------------------------------------------
  -- Delete the contact
  -----------------------------------------------
  DeleteAddressEntry modal -> case modal of
    GetDeleteConfirmation mTarget -> 
      [ Model $ model & #addressBookModel % #deletingContact .~ True -- Show widget.
                      & #addressBookModel % #selectedContact .~ mTarget
      ]
    CancelDeletion -> 
      [ Model $ model & #addressBookModel % #deletingContact .~ False 
                      & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ AddressBookEvent $ DeleteAddressEntry PostDeletionAction) $ do
          -- Get the entry id for the contact to delete.
          let currentId = fromMaybe 0 $
                model ^? #addressBookModel % #selectedContact % _Just % #addressEntryId

          -- Delete the entry.
          deleteAddressEntry databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the entry id for the contact that was deleted.
      let currentId = fromMaybe 0
            $ model ^? #addressBookModel % #selectedContact % _Just % #addressEntryId
          newBook = filter (\b -> b ^. #addressEntryId /= currentId) addressBook
      in [ Model $ model & #addressBookModel % #deletingContact .~ False
                         & #addressBook .~ newBook
                         & #addressBookModel % #selectedContact .~ Nothing
         ]

  -----------------------------------------------
  -- Add new user output to contact
  -----------------------------------------------
  AddNewUserOutputToContact modal -> case modal of
    StartAdding mTarget -> 
      let newOutput = 
            def & #paymentAddress .~ maybe "" toText (mTarget ^? _Just % #paymentAddress)
                & #alias .~ fromMaybe "" (mTarget ^? _Just % #alias)
      in  [ Model $ model & #addressBookModel % #creatingOutput .~ True -- Show widget.
                          & #addressBookModel % #selectedContact .~ mTarget
                          & #addressBookModel % #newUserOutput .~ newOutput
          ]
    CancelAdding -> 
      [ Model $ model & #addressBookModel % #creatingOutput .~ False
                      & #addressBookModel % #newUserOutput .~ def -- Clear for next time.
                      & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AddressBookEvent . AddNewUserOutputToContact . AddResult) $
          fromRightOrAppError $ 
            processNewUserOutput (config ^. #network) (addressBookModel ^. #newUserOutput)
      ]
    AddResult verifiedOutput ->
      -- Get the index for the new output.
      let newIdx = length $ model ^. #txBuilderModel % #userOutputs in
        [ Model $
            model & #addressBookModel % #creatingOutput .~ False
                  & #addressBookModel % #selectedContact .~ Nothing
                  & #addressBookModel % #newUserOutput .~ def
                  & #txBuilderModel % #userOutputs %~ flip snoc (newIdx,verifiedOutput)
        , Task $ return $ Alert "Successfully added to builder!"
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
processNewAddressEntry 
  :: NewAddressEntry 
  -> Network
  -> ProfileId 
  -> AddressEntryId 
  -> [AddressEntry] 
  -> Either Text AddressEntry
processNewAddressEntry NewAddressEntry{..} network profileId addressEntryId book = do
  -- Verify that the alias is not already being used.
  maybeToLeft () $ fmap (const "This alias is already being used for another address.") $ 
    find (\a -> a ^. #alias == alias) book

  newVerifiedEntry <- readPaymentAddress network paymentAddress >>= return . \addr -> AddressEntry
    { profileId = profileId
    , addressEntryId = addressEntryId
    , alias = alias
    , paymentAddress = addr
    }

  return newVerifiedEntry
