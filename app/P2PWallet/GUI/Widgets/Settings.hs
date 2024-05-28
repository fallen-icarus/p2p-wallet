module P2PWallet.GUI.Widgets.Settings where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

settingsWidget :: AppModel -> AppNode
settingsWidget model = do
    zstack
      [ setttingsMenu `nodeVisible` (not isAdding && not isDeleting)
      , editProfileWidget model `nodeVisible` isAdding
      , confirmDeleteWidget model `nodeVisible` isDeleting
      ]
  where
    isAdding :: Bool
    isAdding = model ^. #addingProfile

    isDeleting :: Bool
    isDeleting = model ^. #deletingProfile

    setttingsMenu :: AppNode
    setttingsMenu =
      centerWidget $ vstack_ [childSpacing]
        [ spacer
        , hstack_ [childSpacing]
            [ spacer
            , label "Profile Name:" `styleBasic` [textFont "Italics", textSize 18]
            , flip styleBasic [textSize 18] $ 
                label $ fromMaybe "" $ model ^? #selectedProfile % _Just % #alias
            , button remixEdit2Line (ProfileEvent $ ChangeProfileName StartAdding)
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
            , button remixDeleteBinLine (ProfileEvent $ DeleteProfile GetDeleteConfirmation)
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
        , mainButton "Confirm" $ ProfileEvent $ DeleteProfile ConfirmDeletion
        , spacer
        , button "Cancel" $ ProfileEvent $ DeleteProfile CancelDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]

editProfileWidget :: AppModel -> AppNode
editProfileWidget _ = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Profile Name:"
            , spacer
            , textField (toLensVL $ #extraTextField)
                `styleBasic` [width 500]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , mainButton "Confirm" $ ProfileEvent $ ChangeProfileName ConfirmAdding
        , spacer
        , button "Cancel" $ ProfileEvent $ ChangeProfileName CancelAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]
