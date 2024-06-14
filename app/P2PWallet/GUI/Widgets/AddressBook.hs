{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.AddressBook
  ( 
    addressBookWidget
  ) where

import Monomer
import Data.Text qualified as Text

import P2PWallet.Data.AddressBook
import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

addressBookWidget :: AppModel -> AppNode
addressBookWidget AppModel{..} = do
    zstack
      [ vstack
          [ hstack
              [ label "Contacts"
                  `styleBasic` [padding 10, textSize 20]
              , filler
              , tooltip_ "New Contact" [tooltipDelay 0] $
                  toggleButton_ largeAddIcon
                    (toLensVL $ #addressBookModel % #addingContact)
                    [toggleButtonOffStyle menuOffStyle]
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , paddingT 0
                      , paddingB 0
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , spacer_ [width 3]
          , textField_ 
              (toLensVL $ #addressBookModel % #search) 
              [placeholder "one of: alias, bech32 payment address"] 
              `styleBasic`
                [ textSize 12
                ]

          , widgetIf (sample /= []) $ cushionWidget $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map contactRow sample)
                `styleBasic` 
                  [ padding 10
                  , paddingT 0
                  ]
          , widgetIf (sample == [] && addressBook /= []) $ 
              centerWidget $
                label "No contacts match that search."
                 `styleBasic` [textFont "Italics"]
          , widgetIf (addressBook == []) $
              centerWidget $
                label "This profile does not have any saved contacts."
                 `styleBasic` [textFont "Italics"]
          ] `styleBasic`
              [ padding 20
              , radius 20
              , bgColor customGray2
              ]
            `nodeVisible` (not isAdding && not isEditing && not isDeleting && not isSpending)
      , widgetIf isAdding addContactWidget
      , widgetIf isEditing editContactWidget
      , widgetIf isDeleting $ confirmDeleteWidget $ 
          fromMaybe "" $ addressBookModel ^? #selectedContact % _Just % #alias
      , widgetIf isSpending $ addNewUserOutputWidget $ 
          fromMaybe "" $ addressBookModel ^? #newUserOutput % #alias
      ] `styleBasic`
          [ padding 20
          ]
  where
    isAdding :: Bool
    isAdding = addressBookModel ^. #addingContact

    isEditing :: Bool
    isEditing = addressBookModel ^. #editingContact

    isDeleting :: Bool
    isDeleting = addressBookModel ^. #deletingContact

    isSpending :: Bool
    isSpending = addressBookModel ^. #creatingOutput

    menuOffStyle :: Style
    menuOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1 ]

    searchTarget :: Text
    searchTarget = addressBookModel ^. #search

    searchFilter :: [AddressEntry] -> [AddressEntry]
    searchFilter
      | searchTarget == "" = filter (const True)
      | otherwise = filter $ \AddressEntry{..} -> or
          [ searchTarget `Text.isPrefixOf` alias
          , searchTarget `Text.isPrefixOf` (toText paymentAddress)
          ]

    sample :: [AddressEntry]
    sample = searchFilter addressBook

    contactRow :: AddressEntry -> AppNode
    contactRow a@AddressEntry{..} = do
      vstack
        [ hstack 
            [ label alias
                `styleBasic` [textSize 16]
            , spacer_ [width 5]
            , box_ [onClick $ AddressBookEvent $ AddNewUserOutputToContact $ StartAdding $ Just a] $ 
                label "Send"
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , radius 10
                    , paddingT 3
                    , paddingB 3
                    , paddingR 7
                    , paddingL 7
                    , bgColor customGray2
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , tooltip_ "Delete" [tooltipDelay 0] $
                button deleteIcon 
                    (AddressBookEvent $ DeleteAddressEntry $ GetDeleteConfirmation $ Just a)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 3]
            , tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon 
                    (AddressBookEvent $ ChangeAddressEntry $ StartAdding $ Just a)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        , spacer_ [width 4]
        , hstack
            [ copyableLabelSub (toText paymentAddress)
            , filler
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray4
            , radius 5
            , border 0 black
            ]

addContactWidget :: AppNode
addContactWidget = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Name:"
            , spacer
            , textField_ (toLensVL $ #addressBookModel % #newAddressEntry % #alias) 
                  [placeholder "Alice"]
                `styleBasic` [width 300]
            ]
        , hstack 
            [ label "Payment Address:"
            , spacer
            , textField_ (toLensVL $ #addressBookModel % #newAddressEntry % #paymentAddress) 
                  [placeholder "bech32 address"]
                `styleBasic` [textSize 10]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ AddressBookEvent $ AddNewAddressEntry CancelAdding
        , spacer
        , mainButton "Confirm" $ AddressBookEvent $ AddNewAddressEntry ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

editContactWidget :: AppNode
editContactWidget = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Name:"
            , spacer
            , textField_ (toLensVL $ #addressBookModel % #newAddressEntry % #alias) 
                  [placeholder "Alice"]
                `styleBasic` [width 300]
            ]
        , hstack 
            [ label "Payment Address:"
            , spacer
            , textField_ (toLensVL $ #addressBookModel % #newAddressEntry % #paymentAddress) 
                  [placeholder "bech32 address"]
                `styleBasic` [textSize 10]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ AddressBookEvent $ ChangeAddressEntry CancelAdding
        , spacer
        , mainButton "Confirm" $ AddressBookEvent $ ChangeAddressEntry ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

confirmDeleteWidget :: Text -> AppNode
confirmDeleteWidget alias = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , alias
        , "'?"
        ]
    , hstack 
        [ filler
        , button "Cancel" $ AddressBookEvent $ DeleteAddressEntry CancelDeletion
        , spacer
        , mainButton "Confirm" $ AddressBookEvent $ DeleteAddressEntry ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]


addNewUserOutputWidget :: Text -> AppNode
addNewUserOutputWidget recipient = do
  centerWidget $ vstack
    [ centerWidgetH $ label ("How much would you like to pay " <> recipient <> "?")
    , spacer_ [width 20]
    , hstack
        [ label "ADA:"
        , spacer
        , textField_ (toLensVL $ #addressBookModel % #newUserOutput % #ada)
              [placeholder "1.234567"]
            `styleBasic` [width 200]
        ]
    , spacer
    , hstack
        [ label "Native Assets (separated with newlines)"
        , mainButton helpIcon (Alert nativeAssetAreaEntryMsg)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        ]
    , textArea (toLensVL $ #addressBookModel % #newUserOutput % #nativeAssets)
        `styleBasic` [height 180, textSize 10]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ AddressBookEvent $ AddNewUserOutputToContact CancelAdding
        , spacer
        , mainButton "Confirm" $ AddressBookEvent $ AddNewUserOutputToContact ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSub :: Text -> WidgetNode s AppEvent
copyableLabelSub caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 10
      , border 0 transparent
      , textColor lightGray
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
