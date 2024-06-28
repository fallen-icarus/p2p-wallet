module P2PWallet.GUI.Widgets.Home 
  ( 
    homeWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.GUI.Widgets.Home.About
import P2PWallet.GUI.Widgets.Home.AddPaymentWallet
import P2PWallet.GUI.Widgets.Home.NativeAssets
import P2PWallet.GUI.Widgets.Home.Transactions
import P2PWallet.GUI.Widgets.Home.UTxOs
import P2PWallet.Prelude

homeWidget :: AppModel -> AppNode
homeWidget model@AppModel{..} = do
    zstack
      [ mainWidget model `nodeVisible` (hasPaymentWallets && not isAdding && not isEditing && not isDeleting)
      , addFirstWalletWidget
          `nodeVisible` (not isAdding && not hasPaymentWallets && not isEditing && not isDeleting)
      , widgetIf isAdding $ addPaymentWalletWidget model
      , widgetIf isEditing editPaymentWalletWidget
      , widgetIf isDeleting $ confirmDeleteWidget model
        -- The inspected transaction is here since only one transaction can be inspected at a time.
        -- It doesn't make sense to move to other home scenes while inspecting a transaction.
      , widgetMaybe (model ^. #homeModel % #inspectedTransaction) $ \tx -> 
          inspectionWidget tx model
      ]
  where
    hasPaymentWallets :: Bool
    hasPaymentWallets = not $ null $ knownWallets ^. #paymentWallets

    isAdding :: Bool
    isAdding = homeModel ^. #addingWallet

    isEditing :: Bool
    isEditing = homeModel ^. #editingWallet

    isDeleting :: Bool
    isDeleting = homeModel ^. #deletingWallet

-- The main widget that should only be shown if there are currently tracked payment wallets
-- AND no details need to be shown.
mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{scene=_,..} =
    vstack_ [childSpacing]
      [ spacer
      , centerWidgetH headerWidget
      , centerWidgetH $ hstack 
          [ spacer
          , homeSceneButton "About" HomeAbout
          , spacer
          , separatorLine `styleBasic` [paddingT 5, paddingB 5]
          , spacer
          , homeSceneButton "Transactions" HomeTransactions
          , spacer
          , separatorLine `styleBasic` [paddingT 5, paddingB 5]
          , spacer
          , homeSceneButton "UTxOs" HomeUTxOs
          , spacer
          , separatorLine `styleBasic` [paddingT 5, paddingB 5]
          , spacer
          , homeSceneButton "Native Assets" HomeAssets
          , spacer
          ] `styleBasic`
              [ bgColor customGray2
              , radius 10
              , border 1 black
              ]
      , box_ [mergeRequired reqUpdate] (aboutWidget model)
          `nodeVisible` (homeModel ^. #scene == HomeAbout)
      , box_ [mergeRequired reqUpdate] (transactionsWidget model)
          `nodeVisible` (homeModel ^. #scene == HomeTransactions)
      , box_ [mergeRequired reqUpdate] (utxosWidget model)
          `nodeVisible` (homeModel ^. #scene == HomeUTxOs)
      , box_ [mergeRequired reqUpdate] (nativeAssetsWidget model)
          `nodeVisible` (homeModel ^. #scene == HomeAssets)
      ] 
  where
    homeSceneButton :: Text -> HomeScene -> AppNode
    homeSceneButton caption scene = do
      let dormantColor
            | homeModel ^. #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | homeModel ^. #scene == scene = customBlue
            | otherwise = lightGray
      button caption (HomeEvent $ ChangeHomeScene scene)
        `styleBasic` [bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    -- Don't automatically redraw the UI with every key press with text fields. This could
    -- be laggy and is unecessary. The `forceRedraw` field is used to redraw the UI once the
    -- use is finished entering.
    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old@AppModel{homeModel=oldHome} new@AppModel{homeModel=newHome} 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | old ^. #extraTextField /= new ^. #extraTextField = False
      | oldHome ^. #utxoFilterModel /= def && newHome ^. #utxoFilterModel == def = True
      | oldHome ^. #utxoFilterModel % #search /= newHome ^. #utxoFilterModel % #search = False
      | oldHome ^. #assetFilterModel % #search /= newHome ^. #assetFilterModel % #search = False
      | oldHome ^. #txFilterModel % #search /= newHome ^. #txFilterModel % #search = False
      | oldHome ^. #txFilterModel % #dateRange /= newHome ^. #txFilterModel % #dateRange = False
      | otherwise = True

    (walletTypeTip,walletTypeIcon)
      | isNothing $ homeModel ^. #selectedWallet % #paymentKeyPath = ("Watched", watchedIcon)
      | otherwise = ("Paired", pairedIcon)

    headerWidget :: AppNode
    headerWidget = do
      let innerDormantStyle = 
            def `styleBasic` [bgColor customGray3, border 1 black]
                `styleHover` [bgColor customGray2, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [bgColor customGray3, border 1 customBlue]
                `styleFocusHover` [bgColor customGray2, border 1 customBlue]
      hstack_ [childSpacing]
        [ -- The currently selected wallet's total amount of ADA.
          tooltip_ "Total Balance" [tooltipDelay 0] $
            label $ display $ homeModel ^. #selectedWallet % #lovelace
          -- A dropdown for switching between payment wallets.
        , textDropdown_ 
              (toLensVL $ #homeModel % #selectedWallet) 
              (knownWallets ^. #paymentWallets) 
              (view #alias) -- The dropdown displays the payment wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray3
              , width 150
              , border 1 black
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          -- An icon representing whether the selected wallet is paired or watched.
        , tooltip_ walletTypeTip [tooltipDelay 0] $ label walletTypeIcon
            `styleBasic` [textFont "Remix", paddingT 10]
          -- A button to manually resync the wallets.
        , tooltip_ "Refresh" [tooltipDelay 0] $
            button refreshIcon (SyncWallets StartProcess)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 0
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          -- A popup menu of actions like changing a wallets name.
        , morePopup
        ]

    -- A popup menu for actions on the home scene.
    morePopup :: AppNode
    morePopup = do
      vstack
        [ -- This is the button that opens the popup.
          tooltip_ "More" [tooltipDelay 0] $
            button horizontalMoreIcon (HomeEvent ShowHomeMorePopup)
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
        , customPopup (toLensVL $ #homeModel % #showMorePopup) $
            vstack
              [ button "Add Wallet" (HomeEvent $ PairPaymentWallet $ StartAdding Nothing)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , button "Edit Name" (HomeEvent $ ChangePaymentWalletName $ StartAdding Nothing)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , separatorLine `styleBasic` [fgColor black, padding 5]
              , button "Delete Wallet" (HomeEvent $ DeletePaymentWallet $ GetDeleteConfirmation Nothing)
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

-- A welcome message when there are currently no tracked payment wallets.
addFirstWalletWidget :: AppNode
addFirstWalletWidget =
  vstack
    [ centerWidget $
        flip styleBasic [bgColor transparent, padding 20, radius 5] $ 
          box $ 
            label "Add your first payment wallet to begin!" 
             `styleBasic` [textFont "Italics"]
    , filler
    , hstack
        [ filler
        -- The widget is initialized using PairPaymentWallet. 
        , box (mainButton "Add Wallet" $ HomeEvent $ PairPaymentWallet $ StartAdding Nothing) 
            `styleBasic` [padding 20]
        ]
    ]

-- Change the name for the currently selected wallet.
editPaymentWalletWidget :: AppNode
editPaymentWalletWidget = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Wallet Name:"
            , spacer
            , textField (toLensVL #extraTextField) 
                `styleBasic` [width 400, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ HomeEvent $ ChangePaymentWalletName CancelAdding
        , spacer
        , mainButton "Confirm" $ HomeEvent $ ChangePaymentWalletName ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

-- Delete the currently selected wallet.
confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget AppModel{homeModel} = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , homeModel ^. #selectedWallet % #alias
        , "'?"
        ]
    , hstack 
        [ filler
        , button "Cancel" $ HomeEvent $ DeletePaymentWallet CancelDeletion
        , spacer
        , mainButton "Confirm" $ HomeEvent $ DeletePaymentWallet ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]
