module P2PWallet.GUI.Widgets.Options
  ( 
    optionsWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.GUI.Widgets.Options.Buyer
import P2PWallet.GUI.Widgets.Options.Research
import P2PWallet.GUI.Widgets.Options.Writer
import P2PWallet.Prelude

optionsWidget :: AppModel -> AppNode
optionsWidget model@AppModel{..} = do
    zstack
      [ mainWidget model `nodeVisible` and
          [ hasOptionsWallets
          , not isAdding
          , not isDeleting
          ]
      , addFirstWalletWidget `nodeVisible` and
          [ not isAdding
          , not hasOptionsWallets
          , hasStakeWallets -- Stake wallets need to be added first.
          , not isDeleting
          ]
      , requiresFirstStakeWallet `nodeVisible` and
          [ not isAdding
          , not hasOptionsWallets
          , not hasStakeWallets
          , not isDeleting
          ]
      , widgetIf isAdding $ addOptionsWalletWidget model
      , widgetIf isDeleting $ confirmDeleteWidget model
      ]
  where
    hasOptionsWallets :: Bool
    hasOptionsWallets = knownWallets ^. #optionsWallets /= []

    hasStakeWallets :: Bool
    hasStakeWallets = knownWallets ^. #stakeWallets /= []

    isAdding :: Bool
    isAdding = optionsModel ^. #addingWallet

    isDeleting :: Bool
    isDeleting = optionsModel ^. #deletingWallet

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{scene=_,..} =
    vstack
      [ spacer_ [width 20]
      , centerWidgetH $ hstack
          [ mainSceneMenu
          , spacer_ [width 5]
          , walletMenu
          ]
      , spacer_ [width 5]
      , writerWidget model `nodeVisible` (optionsModel ^. #scene == OptionsWriterScene)
      , buyerWidget model `nodeVisible` (optionsModel ^. #scene == OptionsBuyerScene)
      , researchWidget model `nodeVisible` (optionsModel ^. #scene == OptionsResearchScene)
      ] 
  where
    optionsMainSceneButton :: Text -> OptionsScene -> AppNode
    optionsMainSceneButton caption scene = do
      let dormantColor
            | optionsModel ^. #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | optionsModel ^. #scene == scene = customBlue
            | otherwise = lightGray
      button caption (OptionsEvent $ ChangeOptionsScene scene)
        `styleBasic` [textSize 12, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    (walletTypeTip,walletTypeIcon)
      | isNothing $ optionsModel ^. #selectedWallet % #stakeKeyDerivation = ("Watched", watchedIcon)
      | otherwise = ("Paired", pairedIcon)

    mainSceneMenu :: AppNode
    mainSceneMenu = hstack 
      [ spacer
      , optionsMainSceneButton "Sell" OptionsWriterScene
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , optionsMainSceneButton "Buy" OptionsBuyerScene
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , optionsMainSceneButton "Research" OptionsResearchScene
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , tooltip_ "Refresh Wallets" [tooltipDelay 0] $
          box_ [alignMiddle, onClick $ SyncWallets $ StartProcess Nothing] $
            label refreshIcon
              `styleBasic` 
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray4, cursorIcon CursorHand]
      , spacer
      ] `styleBasic`
          [ bgColor customGray2
          , radius 10
          , border 1 black
          ]

    walletMenu :: AppNode
    walletMenu = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
      hstack
        [ box_ [alignMiddle] $ tooltip_ walletTypeTip [tooltipDelay 0] $ label walletTypeIcon
            `styleBasic` [textFont "Remix", textMiddle]
        , spacer_ [width 5]
        , textDropdown_ 
              (toLensVL $ #optionsModel % #selectedWallet) 
              (knownWallets ^. #optionsWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 12
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 5]
        , box_ [alignMiddle] morePopup
        ] 

confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget model = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , model ^. #optionsModel % #selectedWallet % #alias
        , "'?"
        ]
    , centerWidgetH $ label
        "This will only remove the wallet as an options wallet."
          `styleBasic` [textSize 12, textColor customRed]
    , hstack 
        [ filler
        , button "Cancel" $ OptionsEvent $ DeleteOptionsWallet CancelDeletion
        , spacer
        , mainButton "Confirm" $ OptionsEvent $ DeleteOptionsWallet ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

-- A notice that stake wallets must be added before options wallets.
requiresFirstStakeWallet :: AppNode
requiresFirstStakeWallet =
  centerWidget $ vstack
    [ box_ [alignMiddle] $ label "You must add a stake wallet before you can trade options."
        `styleBasic` [textFont "Italics", textSize 14, textColor lightGray]
    , box_ [alignMiddle] $ label "Go to the 'Staking' page to add one."
        `styleBasic` [textFont "Italics", textSize 14, textColor lightGray]
    ] `styleBasic` 
        [ bgColor customGray3
        , padding 20
        , radius 5
        , border 1 customBlue
        ] 

addOptionsWalletWidget :: AppModel -> AppNode
addOptionsWalletWidget AppModel{knownWallets,optionsModel} = do
  let innerDormantStyle = 
        def `styleBasic` [textSize 12, bgColor customGray3, border 1 black]
            `styleHover` [textSize 12, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 12, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 12, bgColor customGray2, border 1 customBlue]
      maybeLens' = maybeLens def (#optionsModel % #targetStakeCredential)
      targetStakeCredential = optionsModel ^. #targetStakeCredential
      currentOptionsWalletIds = map (view #stakeWalletId) $ knownWallets ^. #optionsWallets
      availWallets = filter ((`notElem` currentOptionsWalletIds) . view #stakeWalletId) $ 
        knownWallets ^. #stakeWallets
      editFields = 
        vstack_ [childSpacing]
          [ centerWidgetH $ hstack 
              [ label "Stake Credential:"
                  `styleBasic` [textSize 14]
              , spacer
              , textDropdown_ 
                    (toLensVL maybeLens')
                    availWallets
                    (view #alias)
                    [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                  `styleBasic` [width 200, bgColor customGray1]
                  `styleFocus` [border 1 customBlue]
              , spacer
              , box_ [onClick $ Alert defiStakeCredentialMsg] $ label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 14
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , separatorLine `styleBasic` [paddingL 50, paddingR 50]
          , widgetMaybe targetStakeCredential $ \StakeWallet{stakeAddress,stakeKeyDerivation} -> 
              vstack
                [ spacer
                , hstack
                    [ label "Stake Address:"
                        `styleBasic` [textSize 12]
                    , spacer
                    , label (toText stakeAddress)
                        `styleBasic` [textSize 12]
                    ]
                , spacer
                , widgetMaybe stakeKeyDerivation $ \keyInfo -> hstack
                    [ label "Derivation:"
                        `styleBasic` [textSize 12]
                    , spacer
                    , label (display keyInfo)
                        `styleBasic` [textSize 12]
                    ]
                ]
          ]
  centerWidget $ vstack 
    [ widgetIf (isJust targetStakeCredential) editFields
    -- This should only appear when there are tracked options wallets but no stake wallets; all of the
    -- stake wallets would have been deleted.
    , widgetIf (isNothing targetStakeCredential) $ centerWidgetH $ 
        label
          "You must first add the staking credential you wish to use from the `Staking` page."
          `styleBasic` [textSize 14]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ OptionsEvent $ AddNewOptionsWallet CancelAdding
        , spacer
        , mainButton "Confirm" (OptionsEvent $ AddNewOptionsWallet ConfirmAdding)
            `nodeEnabled` isJust targetStakeCredential
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

-- A welcome message when there are currently no tracked wallets.
addFirstWalletWidget :: AppNode
addFirstWalletWidget =
  vstack
    [ centerWidget $
        flip styleBasic [bgColor transparent, padding 20, radius 5] $ 
          box $ 
            label "Add your first options wallet to begin!" 
             `styleBasic` [textFont "Italics"]
    , filler
    , hstack
        [ filler
        , box (mainButton "Add Wallet" $ OptionsEvent $ AddNewOptionsWallet $ StartAdding Nothing) 
            `styleBasic` [padding 20]
        ]
    ]

-- A popup menu for actions on the Options page.
morePopup :: AppNode
morePopup = do
  vstack
    [ -- This is the button that opens the popup.
      tooltip_ "More" [tooltipDelay 0] $
        box_ [onClick $ OptionsEvent ShowOptionsMorePopup] $ label horizontalMoreIcon
          `styleBasic`
            [ border 0 transparent
            , radius 20
            , paddingL 0
            , paddingR 0
            , bgColor transparent
            , textColor customBlue
            , textMiddle
            , textFont "Remix"
            ]
          `styleHover` [bgColor customGray2, cursorIcon CursorHand]
      -- This is the popup menu. The menu will be closed upon choosing an action. The menu
      -- can also be closed by clicking outside of the menu's area.
    , customPopup (toLensVL $ #optionsModel % #showMorePopup) $
        vstack
          [ button "Add Wallet" (OptionsEvent $ AddNewOptionsWallet $ StartAdding Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , separatorLine `styleBasic` [fgColor black, padding 5]
          , button "Delete Wallet" (OptionsEvent $ DeleteOptionsWallet $ GetDeleteConfirmation Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customRed
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          ] `styleBasic`
              [ bgColor customGray3
              , border 1 black
              , padding 5
              ]
    ] `styleBasic` [paddingL 0, paddingR 0]