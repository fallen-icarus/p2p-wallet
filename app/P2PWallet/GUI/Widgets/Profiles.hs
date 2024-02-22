module P2PWallet.GUI.Widgets.Profiles where

import Monomer
import Monomer.Lens qualified as L
import Prettyprinter (tupled)

import P2PWallet.Data.App
import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

profilesWidget :: AppWenv -> AppModel -> AppNode
profilesWidget wenv model = do
    zstack
      [ profilePickerWidget `nodeVisible` (not isAdding)
      , addProfileWidget wenv model `nodeVisible` isAdding
      ]

  where
    isAdding :: Bool
    isAdding = model ^. addingProfile

    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    sectionBgColor :: Color
    sectionBgColor = wenv ^. L.theme . L.sectionColor

    profileColumn :: Profile -> AppNode
    profileColumn p =
      let accMsg = fromString $ printf "Account ID: %d" $ unAccountIndex $ p ^. accountIndex
          content =
            vstack 
              [ centerWidgetH $ label (p ^. alias) 
                  `styleBasic` [textFont "Medium", textSize 24, paddingT 10]
              , centerWidgetH $ flip styleBasic [textSize 14] $ label $ 
                  show $ tupled $ [accMsg]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick (ChangeProfile $ LoadNewProfile p)] content 
           `styleBasic` [bgColor sectionBgColor, radius 5]

    newProfileBox :: AppNode
    newProfileBox =
      let content =
            vstack
              [ centerWidgetH $ label remixUserAddLine
                  `styleBasic` [textFont "Remix", textSize 24, paddingT 10]
              , centerWidgetH $ label "New Profile" `styleBasic` [textFont "Medium", textSize 20]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick $ AddNewProfile StartAdding] content 
           `styleBasic` [bgColor sectionBgColor, radius 5]

    profilePickerWidget :: AppNode
    profilePickerWidget = centerWidget $
      vstack
        [ centerWidgetH $ label "Select a profile:" `styleBasic` [textFont "Medium", textSize 30]
        , spacer_ [width 100]
        , centerWidgetH $ flip styleBasic [border 1 black, padding 10, radius 5] $ 
            hstack_ [childSpacing] $ 
              map profileColumn (model ^. knownProfiles) <> [ newProfileBox ]
        ]

addProfileWidget :: AppWenv -> AppModel -> AppNode
addProfileWidget wenv _ = do
  let sectionBg = wenv ^. L.theme . L.sectionColor
      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Profile Name:"
              , spacer
              , textField_ (newProfile . alias) [placeholder "Personal"] 
                  `styleBasic` [width 500]
              ]
        , hstack
            [ label "Hardware Wallet:"
            , spacer
            , textDropdown_ (newProfile . device) supportedDevices showHwDevice []
                `styleBasic` [width 200]
            ]
          , hstack 
              [ label "Account ID:"
              , spacer
              , numericField_ (newProfile . accountIndex) [decimals 0]
                  `styleBasic` [width 100]
              ]
          ]

  centerWidget $ vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Confirm" $ AddNewProfile ConfirmAdding
        , spacer
        , button "Cancel" $ AddNewProfile CancelAdding
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20, width 700]
