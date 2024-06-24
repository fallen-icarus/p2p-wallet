{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.AddressBookModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.UserOutput
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Prelude

-------------------------------------------------
-- Address Book Events
-------------------------------------------------
-- | The possible UI events on the Address Book page.
data AddressBookEvent
  -- | Add a new contact to the address book.
  = AddNewAddressEntry (AddEvent AddressEntry AddressEntry)
  -- | Edit the selected contact.
  | ChangeAddressEntry (AddEvent AddressEntry AddressEntry)
  -- | Delete the selected contact.
  | DeleteAddressEntry (DeleteWithConfirmationEvent AddressEntry)
  -- | Add an output to the specified contact to the tx builder.
  | AddNewUserOutputToContact (AddEvent AddressEntry UserOutput)

-------------------------------------------------
-- Address Book Model
-------------------------------------------------
data AddressBookModel = AddressBookModel
  -- | The targets to search for.
  { search :: Text
  -- | The new address book entry.
  , newAddressEntry :: NewAddressEntry
  -- | Whether the add new address widget should be open.
  , addingContact :: Bool
  -- | Whether the edit address widget should be open.
  , editingContact :: Bool
  -- | Whether the delete address widget should be open.
  , deletingContact :: Bool
  -- | The address book entry being edited/deleted.
  , selectedContact :: Maybe AddressEntry
  -- | Whether a new output to the contact is being created.
  , creatingOutput :: Bool
  -- | The new user output to the specified contact.
  , newUserOutput :: NewUserOutput
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AddressBookModel

instance Default AddressBookModel where
  def = AddressBookModel 
    { search = ""
    , newAddressEntry = def
    , addingContact = False
    , editingContact = False
    , deletingContact = False
    , selectedContact = Nothing
    , creatingOutput = False
    , newUserOutput = def
    }
