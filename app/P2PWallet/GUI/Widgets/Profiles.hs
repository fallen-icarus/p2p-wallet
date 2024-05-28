module P2PWallet.GUI.Widgets.Profiles where

import Monomer
import Prettyprinter (tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Profile
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Information
import P2PWallet.Prelude

profilesWidget :: AppModel -> AppNode
profilesWidget model = do
    zstack
      [ profilePickerWidget `nodeVisible` (not isAdding)
      , addProfileWidget model `nodeVisible` isAdding
      ]

  where
    isAdding :: Bool
    isAdding = model ^. #addingProfile

    profileColumn :: Profile -> AppNode
    profileColumn p =
      let accMsg = fromString $ printf "Account ID: %d" $ p ^. #accountIndex % #unAccountIndex
          content =
            vstack 
              [ centerWidgetH $ label (p ^. #alias) 
                  `styleBasic` [textFont "Medium", textSize 24, paddingT 10]
              , centerWidgetH $ flip styleBasic [textSize 14] $ label $ 
                  show $ tupled $ [accMsg]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
      in box_ [expandContent, onClick $ ProfileEvent $ LoadSelectedProfile p] content 
           `styleBasic` [bgColor customGray1, radius 5]

    newProfileBox :: AppNode
    newProfileBox =
      let content =
            vstack
              [ centerWidgetH $ label remixUserAddLine
                  `styleBasic` [textFont "Remix", textSize 24, paddingT 10]
              , centerWidgetH $ label "New Profile" `styleBasic` [textFont "Medium", textSize 20]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
      in box_ [expandContent, onClick $ ProfileEvent $ AddNewProfile StartAdding] content 
           `styleBasic` [bgColor customGray1, radius 5]

    profilePickerWidget :: AppNode
    profilePickerWidget = centerWidget $
      vstack
        [ filler
        , centerWidgetH $ label "Select a profile:" 
            `styleBasic` [textFont "Medium", textSize 30]
        , spacer_ [width 100]
        , hstack
            [ spacer_ [width 50]
            , flip styleBasic [height 150] $ hscroll $ centerWidget $ 
                flip styleBasic [border 1 black, padding 10, radius 5] $ 
                  hstack_ [childSpacing] $ 
                    map profileColumn (model ^. #knownProfiles) <> [ newProfileBox ]
            , spacer_ [width 50]
            ]
        , filler
        , centerWidgetH $ (button "Switch Networks" $ ChangeMainScene NetworksScene)
            `styleBasic`
              [ bgColor customGray3
              , textColor customBlue
              , border 0 transparent
              , textMiddle
              , textFont "Regular"
              , padding 10
              , radius 10
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        ]

addProfileWidget :: AppModel -> AppNode
addProfileWidget _ = do
  let editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Profile Name:"
              , spacer
              , textField_ (toLensVL $ #newProfile % #alias) [placeholder "Personal"] 
                  `styleBasic` [width 500]
              ]
        , hstack
            [ label "Hardware Wallet:"
            , spacer
            , textDropdown_ (toLensVL $ #newProfile % #device) supportedDevices showHwDevice []
                `styleBasic` [width 200]
            ]
          , hstack 
              [ label "Account ID:"
              , spacer
              , numericField_ (toLensVL $ #newProfile % #accountIndex) [decimals 0]
                  `styleBasic` [width 100]
              , mainButton remixInformationLine (Alert accountIdInfoMsg)
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
          ]

  centerWidget $ vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Confirm" $ ProfileEvent $ AddNewProfile ConfirmAdding
        , spacer
        , button "Cancel" $ ProfileEvent $ AddNewProfile CancelAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]
