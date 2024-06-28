module P2PWallet.GUI.Widgets.Settings 
  ( 
    settingsWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

settingsWidget :: AppModel -> AppNode
settingsWidget model@AppModel{..} = do
    zstack
      [ setttingsMenu `nodeVisible` (not isAdding && not isDeleting)
      , editProfileWidget model `nodeVisible` isAdding
      , confirmDeleteWidget model `nodeVisible` isDeleting
      ]
  where
    isAdding :: Bool
    isAdding = profileModel ^. #addingProfile

    isDeleting :: Bool
    isDeleting = profileModel ^. #deletingProfile

    setttingsMenu :: AppNode
    setttingsMenu =
      centerWidget $ vstack_ [childSpacing]
        [ spacer
        , hstack_ [childSpacing]
            [ spacer
            , label "Profile Name:" `styleBasic` [textFont "Italics", textSize 18]
            , flip styleBasic [textSize 18] $ 
                label $ fromMaybe "" $ model ^? #selectedProfile % _Just % #alias
            , tooltip_ "Change Profile Name" [tooltipDelay 0] $
                button editIcon (ProfileEvent $ ChangeProfileName $ StartAdding Nothing)
                  `styleBasic`
                    [ textFont "Remix"
                    , padding 0
                    , radius 5
                    , textMiddle
                    , border 0 transparent
                    , textColor customBlue
                    , bgColor transparent
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , tooltip_ "Delete Profile" [tooltipDelay 0] $
                button deleteIcon (ProfileEvent $ DeleteProfile $ GetDeleteConfirmation Nothing)
                  `styleBasic`
                    [ textFont "Remix"
                    , padding 0
                    , radius 5
                    , textMiddle
                    , border 0 transparent
                    , textColor customRed
                    , bgColor transparent
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , filler
            ]
        , separatorLine `styleBasic` [paddingL 30, paddingR 30]
        , filler
        ] `styleBasic`
            [ padding 10
            , radius 20
            , bgColor customGray3
            ]

confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget model = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , fromMaybe "" $ model ^? #selectedProfile % _Just % #alias
        , "'?"
        ]
    , hstack 
        [ filler
        , button "Cancel" $ ProfileEvent $ DeleteProfile CancelDeletion
        , spacer
        , mainButton "Confirm" $ ProfileEvent $ DeleteProfile ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

editProfileWidget :: AppModel -> AppNode
editProfileWidget _ = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Profile Name:"
            , spacer
            , textField (toLensVL $ #profileModel % #newProfile % #alias)
                `styleBasic` [width 300, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ ProfileEvent $ ChangeProfileName CancelAdding
        , spacer
        , mainButton "Confirm" $ ProfileEvent $ ChangeProfileName ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]
