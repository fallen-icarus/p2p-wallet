module P2PWallet.GUI.EventHandler.AddressBookEvent
  ( 
    handleAddressBookEvent
  ) where

import Monomer

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Profile
import P2PWallet.Prelude

handleAddressBookEvent :: AppModel -> AddressBookEvent -> [AppEventResponse AppModel AppEvent]
handleAddressBookEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Add new contact
  -----------------------------------------------
  AddNewAddressEntry modal -> case modal of
    StartAdding _ -> 
      [ Model $ model 
          & #addressBookModel % #addingContact .~ True -- Show widget.
          & #addressBookModel % #newAddressEntry .~ def
      ]
    CancelAdding -> 
      [ Model $ model 
          & #addressBookModel % #addingContact .~ False
          & #addressBookModel % #newAddressEntry .~ def -- Clear for next time.
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AddressBookEvent . AddNewAddressEntry . AddResult) $ do
          let Profile{profileId} = fromMaybe def selectedProfile
              newEntry = addressBookModel ^. #newAddressEntry
              network = config ^. #network

          -- Get the new address entry id for the new entry into the address_book table.
          entryId <- getNextContactId databaseFile >>= fromRightOrAppError
          
          -- Validate the new contact info.
          newVerifiedEntry <- fromRightOrAppError $
            verifyNewAddressEntry newEntry network profileId entryId addressBook

          -- Add the new entry to the database.
          insertAddressEntry databaseFile [newVerifiedEntry] >>= fromRightOrAppError

          return newVerifiedEntry
      ]
    AddResult verifiedEntry ->
      [ Model $ model 
          & #addressBook %~ sortOn (view #alias) . flip snoc verifiedEntry
          & #addressBookModel % #addingContact .~ False
          & #addressBookModel % #newAddressEntry .~ def -- Clear information.
      ]

  -----------------------------------------------
  -- Edit the contact
  -----------------------------------------------
  ChangeAddressEntry modal -> case modal of
    StartAdding mTarget -> 
      let newEntry = def 
            & #alias .~ fromMaybe "" (mTarget ^? _Just % #alias)
            & #paymentAddress .~ maybe "" toText (mTarget ^? _Just % #paymentAddress)
      in  [ Model $ model 
              & #addressBookModel % #editingContact .~ True -- Show widget.
              & #addressBookModel % #selectedContact .~ mTarget
              & #addressBookModel % #newAddressEntry .~ newEntry
          ]
    CancelAdding -> 
      [ Model $ model 
          & #addressBookModel % #editingContact .~ False 
          & #addressBookModel % #newAddressEntry .~ def -- Clear for next time.
          & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AddressBookEvent . ChangeAddressEntry . AddResult) $ do
          let Profile{profileId} = fromMaybe def selectedProfile
              newEntry = addressBookModel ^. #newAddressEntry
              network = config ^. #network
              targetId = fromMaybe 0 $ 
                addressBookModel ^? #selectedContact % _Just % #contactId
              restOfBook = filter (\b -> b ^. #contactId /= targetId) addressBook

          -- Validate the new contact info.
          newVerifiedEntry <- fromRightOrAppError $
            verifyNewAddressEntry newEntry network profileId targetId restOfBook

          -- Add the new entry to the database.
          insertAddressEntry databaseFile [newVerifiedEntry] >>= fromRightOrAppError

          return newVerifiedEntry
      ]
    AddResult AddressEntry{alias,paymentAddress} ->
      let target@AddressEntry{contactId} = 
            fromMaybe def $ addressBookModel ^. #selectedContact
          restOfBook = filter (\b -> b ^. #contactId /= contactId) addressBook
          newInfo = target 
            & #alias .~ alias
            & #paymentAddress .~ paymentAddress
          newBook = sortOn (view #alias) $ newInfo : restOfBook
      in  [ Model $ model 
              & #addressBook .~ newBook
              & #addressBookModel % #editingContact .~ False
              & #addressBookModel % #newAddressEntry .~ def -- Clear information.
              & #addressBookModel % #selectedContact .~ Nothing
          ]

  -----------------------------------------------
  -- Delete the contact
  -----------------------------------------------
  DeleteAddressEntry modal -> case modal of
    GetDeleteConfirmation mTarget -> 
      [ Model $ model 
          & #addressBookModel % #deletingContact .~ True -- Show widget.
          & #addressBookModel % #selectedContact .~ mTarget
      ]
    CancelDeletion -> 
      [ Model $ model 
          & #addressBookModel % #deletingContact .~ False 
          & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ AddressBookEvent $ DeleteAddressEntry PostDeletionAction) $ do
          -- Get the entry id for the contact to delete.
          let currentId = fromMaybe 0 $
                model ^? #addressBookModel % #selectedContact % _Just % #contactId

          -- Delete the entry.
          deleteAddressEntry databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the entry id for the contact that was deleted.
      let currentId = fromMaybe 0
            $ model ^? #addressBookModel % #selectedContact % _Just % #contactId
          newBook = filter (\b -> b ^. #contactId /= currentId) addressBook
      in  [ Model $ model 
              & #addressBookModel % #deletingContact .~ False
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
      in  [ Model $ model 
              & #addressBookModel % #creatingOutput .~ True -- Show widget.
              & #addressBookModel % #selectedContact .~ mTarget
              & #addressBookModel % #newUserOutput .~ newOutput
          ]
    CancelAdding -> 
      [ Model $ model 
          & #addressBookModel % #creatingOutput .~ False
          & #addressBookModel % #newUserOutput .~ def -- Clear for next time.
          & #addressBookModel % #selectedContact .~ Nothing
      ]
    ConfirmAdding -> 
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (AddressBookEvent . AddNewUserOutputToContact . AddResult) $ do
          verifiedOutput <- 
            fromRightOrAppError $ 
              verifyNewUserOutput 
                (config ^. #network) 
                tickerMap
                fingerprintMap
                (addressBookModel ^. #newUserOutput)

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^. #parameters) 
                verifiedOutput

          when (minUTxOValue > verifiedOutput ^. #lovelace) $
            throwIO $ AppError $ minUTxOErrorMessage minUTxOValue

          return verifiedOutput
      ]
    AddResult verifiedOutput ->
      -- Get the index for the new output.
      let newIdx = length $ model ^. #txBuilderModel % #userOutputs in
        [ Model $ model 
            & #waitingStatus % #addingToBuilder .~ False
            & #addressBookModel % #creatingOutput .~ False
            & #addressBookModel % #selectedContact .~ Nothing
            & #addressBookModel % #newUserOutput .~ def
            & #txBuilderModel % #userOutputs %~ flip snoc (newIdx,verifiedOutput)
            & #txBuilderModel %~ balanceTx
        , Task $ return $ Alert "Successfully added to builder!"
        ]
