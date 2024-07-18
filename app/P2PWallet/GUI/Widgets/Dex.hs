module P2PWallet.GUI.Widgets.Dex
  ( 
    dexWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Dex.Positions
import P2PWallet.GUI.Widgets.Dex.Trade
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Prelude

dexWidget :: AppModel -> AppNode
dexWidget model@AppModel{..} = do
    zstack
      [ mainWidget model `nodeVisible` and
          [ hasDexWallets
          , not isAdding
          , not isDeleting
          ]
      , addFirstWalletWidget `nodeVisible` and
          [ not isAdding
          , not hasDexWallets
          , hasStakeWallets -- Stake wallets need to be added first.
          , not isDeleting
          ]
      , requiresFirstStakeWallet `nodeVisible` and
          [ not isAdding
          , not hasDexWallets
          , not hasStakeWallets
          , not isDeleting
          ]
      , widgetIf isAdding $ addDexWalletWidget model
      , widgetIf isDeleting $ confirmDeleteWidget model
      ]
  where
    hasDexWallets :: Bool
    hasDexWallets = knownWallets ^. #dexWallets /= []

    hasStakeWallets :: Bool
    hasStakeWallets = knownWallets ^. #stakeWallets /= []

    isAdding :: Bool
    isAdding = dexModel ^. #addingWallet

    isDeleting :: Bool
    isDeleting = dexModel ^. #deletingWallet

-- The main widget that should only be shown if there are currently tracked swap wallets
-- AND no details need to be shown.
mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{scene=_,..} =
    vstack_ [childSpacing]
      [ spacer
      , hstack
          [ spacer
          , sceneMenu
          , filler
          , walletMenu
          , spacer
          ]
      , box_ [mergeRequired reqUpdate] (positionsWidget model)
          `nodeVisible` (dexModel ^. #scene == DexPositions)
      , box_ [mergeRequired reqUpdate] (tradeWidget model)
          `nodeVisible` (dexModel ^. #scene == DexMarket)
      ] 
  where
    dexSceneButton :: Text -> DexScene -> AppNode
    dexSceneButton caption scene = do
      let dormantColor
            | dexModel ^. #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | dexModel ^. #scene == scene = customBlue
            | otherwise = lightGray
      button caption (DexEvent $ ChangeDexScene scene)
        `styleBasic` [textSize 14, bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{dexModel=oldDex} new@AppModel{dexModel=newDex} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | oldDex ^. #positionsFilterModel % #offerAsset /= 
          newDex ^. #positionsFilterModel % #offerAsset = False
      | oldDex ^. #positionsFilterModel % #askAsset /= 
          newDex ^. #positionsFilterModel % #askAsset = False
      | otherwise = True

    (walletTypeTip,walletTypeIcon)
      | isNothing $ dexModel ^. #selectedWallet % #stakeKeyPath = ("Watched", watchedIcon)
      | otherwise = ("Paired", pairedIcon)

    sceneMenu :: AppNode
    sceneMenu = hstack 
      [ spacer
      , dexSceneButton "Open Positions" DexPositions
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , dexSceneButton "Trade" DexMarket
      , spacer
      , separatorLine `styleBasic` [paddingT 5, paddingB 5]
      , spacer
      , tooltip_ "Refresh Wallets" [tooltipDelay 0] $
          box_ [alignMiddle, onClick $ SyncWallets StartProcess] $
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
            def `styleBasic` [textSize 14, bgColor customGray2, border 1 black]
                `styleHover` [textSize 14, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 14, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 14, bgColor customGray1, border 1 customBlue]
      hstack
        [ box_ [alignMiddle] $ tooltip_ walletTypeTip [tooltipDelay 0] $ label walletTypeIcon
            `styleBasic` [textFont "Remix", textMiddle]
        , spacer_ [width 5]
        , textDropdown_ 
              (toLensVL $ #dexModel % #selectedWallet) 
              (knownWallets ^. #dexWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 14
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 5]
        , box_ [alignMiddle] morePopup
        ] 

-- A popup menu for actions on the DEX scene.
morePopup :: AppNode
morePopup = do
  vstack
    [ -- This is the button that opens the popup.
      tooltip_ "More" [tooltipDelay 0] $
        box_ [onClick $ DexEvent ShowDexMorePopup] $ label horizontalMoreIcon
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
    , customPopup (toLensVL $ #dexModel % #showMorePopup) $
        vstack
          [ button "Add Wallet" (DexEvent $ AddNewDexWallet $ StartAdding Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , separatorLine `styleBasic` [fgColor black, padding 5]
          , button "Delete Wallet" (DexEvent $ DeleteDexWallet $ GetDeleteConfirmation Nothing)
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

-- A welcome message when there are currently no tracked wallets.
addFirstWalletWidget :: AppNode
addFirstWalletWidget =
  vstack
    [ centerWidget $
        flip styleBasic [bgColor transparent, padding 20, radius 5] $ 
          box $ 
            label "Add your first DEX wallet to begin!" 
             `styleBasic` [textFont "Italics"]
    , filler
    , hstack
        [ filler
        , box (mainButton "Add Wallet" $ DexEvent $ AddNewDexWallet $ StartAdding Nothing) 
            `styleBasic` [padding 20]
        ]
    ]

-- A notice that stake wallets must be added before swap wallets.
requiresFirstStakeWallet :: AppNode
requiresFirstStakeWallet =
  centerWidget $ vstack
    [ box_ [alignMiddle] $ label "You must add a stake wallet before you can use the DEX."
        `styleBasic` [textFont "Italics", textSize 14, textColor lightGray]
    , box_ [alignMiddle] $ label "Go to the 'Staking' page to add one."
        `styleBasic` [textFont "Italics", textSize 14, textColor lightGray]
    ] `styleBasic` 
        [ bgColor customGray3
        , padding 20
        , radius 5
        , border 1 customBlue
        ] 

addDexWalletWidget :: AppModel -> AppNode
addDexWalletWidget AppModel{knownWallets,dexModel} = do
  let innerDormantStyle = 
        def `styleBasic` [textSize 12, bgColor customGray3, border 1 black]
            `styleHover` [textSize 12, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 12, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 12, bgColor customGray2, border 1 customBlue]
      maybeLens' = maybeLens def (#dexModel % #newSwapCredential)
      newSwapCredential = dexModel ^. #newSwapCredential
      currentDexWalletIds = map (view #stakeId) $ knownWallets ^. #dexWallets
      availWallets = filter ((`notElem` currentDexWalletIds) . view #stakeId) $ 
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
          , widgetMaybe newSwapCredential $ \StakeWallet{stakeAddress,stakeKeyPath} -> vstack
              [ spacer
              , hstack
                  [ label "Stake Address:"
                      `styleBasic` [textSize 12]
                  , spacer
                  , label (toText stakeAddress)
                      `styleBasic` [textSize 12]
                  ]
              , spacer
              , widgetMaybe stakeKeyPath $ \keyPath -> hstack
                  [ label "Derivation Path:"
                      `styleBasic` [textSize 12]
                  , spacer
                  , label (display keyPath)
                      `styleBasic` [textSize 12]
                  ]
              ]
          ]

  centerWidget $ vstack 
    [ widgetIf (isJust newSwapCredential) editFields
    -- This should only appear when there are tracked swap wallets but no stake wallets; all of the
    -- stake wallets would have been deleted.
    , widgetIf (isNothing newSwapCredential) $ centerWidgetH $ 
        label
          "You must first add the staking credential you wish to use from the `Staking` page."
          `styleBasic` [textSize 14]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DexEvent $ AddNewDexWallet CancelAdding
        , spacer
        , mainButton "Confirm" (DexEvent $ AddNewDexWallet ConfirmAdding)
            `nodeEnabled` isJust newSwapCredential
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget model = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , model ^. #dexModel % #selectedWallet % #alias
        , "'?"
        ]
    , centerWidgetH $ label
        "This will only remove the wallet as a DEX wallet."
          `styleBasic` [textSize 12, textColor customRed]
    , hstack 
        [ filler
        , button "Cancel" $ DexEvent $ DeleteDexWallet CancelDeletion
        , spacer
        , mainButton "Confirm" $ DexEvent $ DeleteDexWallet ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]
