module P2PWallet.GUI.Widgets.Profiles 
  ( 
    profilesWidget
  ) where

import Monomer
import Prettyprinter (tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal.HardwareDevice
import P2PWallet.Data.Core.Profile
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

profilesWidget :: AppModel -> AppNode
profilesWidget model@AppModel{..} = do
    zstack
      [ profilePickerWidget `nodeVisible` not isAdding
      , addProfileWidget model `nodeVisible` isAdding
      ]

  where
    isAdding :: Bool
    isAdding = profileModel ^. #addingProfile

    profileColumn :: Profile -> AppNode
    profileColumn p =
      let accMsg = fromString $ printf "Account ID: %d" $ p ^. #accountIndex % #unAccountIndex
          content =
            vstack 
              [ centerWidgetH $ label (p ^. #alias) 
                  `styleBasic` [textFont "Medium", textSize 24, paddingT 10]
              , centerWidgetH $ label (show $ tupled [accMsg])
                  `styleBasic` [textSize 14]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
      in box_ [expandContent, onClick $ ProfileEvent $ LoadSelectedProfile p] content 
           `styleBasic` [bgColor customGray1, radius 5]

    newProfileBox :: AppNode
    newProfileBox =
      let content =
            vstack
              [ centerWidgetH $ label newUserIcon
                  `styleBasic` [textFont "Remix", textSize 24, paddingT 10]
              , centerWidgetH $ label "New Profile" `styleBasic` [textFont "Medium", textSize 20]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
      in box_ [expandContent, onClick $ ProfileEvent $ AddNewProfile $ StartAdding Nothing] content 
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
                    map profileColumn (profileModel ^. #knownProfiles) <> [ newProfileBox ]
            , spacer_ [width 50]
            ]
        , filler
        , centerWidgetH $ button "Switch Networks" (ChangeMainScene NetworksScene)
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
              , textField_ (toLensVL $ #profileModel % #newProfile % #alias) [placeholder "Personal"] 
                  `styleBasic` [width 300]
              ]
        , hstack
            [ label "Hardware Wallet:"
            , spacer
            , textDropdown_ (toLensVL $ #profileModel % #newProfile % #device) supportedDevices display []
                `styleBasic` [width 200]
            ]
          , hstack 
              [ label "Account ID:"
              , spacer
              , numericField_ (toLensVL $ #profileModel % #newProfile % #accountIndex) [decimals 0]
                  `styleBasic` [width 100]
              , mainButton helpIcon (Alert accountIdInfoMsg)
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
        , button "Cancel" $ ProfileEvent $ AddNewProfile CancelAdding
        , spacer
        , mainButton "Confirm" $ ProfileEvent $ AddNewProfile ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]
