module P2PWallet.GUI.Widgets.Home 
  ( 
    homeWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
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
homeWidget model = do
    zstack
      [ mainWidget `nodeVisible` (hasPaymentWallets && not isAdding && not isEditing && not isDeleting)
      , addFirstWalletWidget 
          `nodeVisible` (not isAdding && not hasPaymentWallets && not isEditing && not isDeleting)
      , widgetIf isAdding $ addPaymentWalletWidget model
      , widgetIf isEditing $ editPaymentWalletWidget model
      , widgetIf isDeleting $ confirmDeleteWidget model
        -- The inspected transaction is here since only one transaction can be inspected at a time.
        -- It doesn't make sense to move to other home scenes while inspecting a transaction.
      , widgetMaybe (model ^. #homeModel % #inspectedTransaction) $ \tx -> 
          inspectionWidget tx model
      ]
  where
    homeSceneButton :: Text -> HomeScene -> AppNode
    homeSceneButton caption scene = do
      let dormantColor
            | model ^. #homeModel % #scene == scene = customBlue
            | otherwise = white
          hoverColor
            | model ^. #homeModel % #scene == scene = customBlue
            | otherwise = lightGray
      button caption (HomeEvent $ ChangeHomeScene scene)
        `styleBasic` [bgColor transparent, textColor dormantColor, border 0 transparent]
        `styleHover` [bgColor transparent, textColor hoverColor]

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old new 
      | old ^. #forceRedraw /= new ^. #forceRedraw = True
      | old ^. #extraTextField /= 
        new ^. #extraTextField = False
      | old ^. #homeModel % #utxoFilterModel /= def && new ^. #homeModel % #utxoFilterModel == def = True
      | old ^. #homeModel % #utxoFilterModel % #search /= 
        new ^. #homeModel % #utxoFilterModel % #search = False
      | old ^. #homeModel % #assetFilterModel % #search /= 
        new ^. #homeModel % #assetFilterModel % #search = False
      | old ^. #homeModel % #txFilterModel % #search /= 
        new ^. #homeModel % #txFilterModel % #search = False
      | old ^. #homeModel % #txFilterModel % #dateRange /= 
        new ^. #homeModel % #txFilterModel % #dateRange = False
      | otherwise = True

    -- Shows an icon representing where the address is paired or watched.
    walletTypeLabel :: AppNode
    walletTypeLabel
      | isNothing $ model ^. #homeModel % #selectedWallet % #paymentKeyPath = 
          tooltip_ "Watched" [tooltipDelay 0] $ label watchedIcon
            `styleBasic` [textFont "Remix", paddingT 10]
      | otherwise = 
          tooltip_ "Paired" [tooltipDelay 0] $ label pairedIcon
            `styleBasic` [textFont "Remix", paddingT 10]

    headerWidget :: AppNode
    headerWidget = do
      let innerDormantStyle = 
            def `styleBasic` [bgColor customGray3, border 1 black]
                `styleHover` [bgColor customGray2, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [bgColor customGray3, border 1 customBlue]
                `styleFocusHover` [bgColor customGray2, border 1 customBlue]
      hstack
        [ label $ fromString $ printf "%D ADA" $ toAda $
            model ^. #homeModel % #selectedWallet % #lovelace
        , spacer
        , textDropdown_ 
              (toLensVL $ #homeModel % #selectedWallet) 
              (model ^. #knownWallets % #paymentWallets) 
              (view #alias) 
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray3
              , width 150
              , border 1 black
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer
        , walletTypeLabel 
        , spacer
        , tooltip_ "Refresh" [tooltipDelay 0] $
            button refreshIcon (SyncWallets StartSync)
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
        , spacer
        , morePopup model
        ]

    -- The main widget that should only be shown if there are currently tracked payment wallets
    -- AND no details need to be shown.
    mainWidget :: AppNode
    mainWidget =
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
            `nodeVisible` (model ^. #homeModel % #scene == HomeAbout)
        , box_ [mergeRequired reqUpdate] (transactionsWidget model)
            `nodeVisible` (model ^. #homeModel % #scene == HomeTransactions)
        , box_ [mergeRequired reqUpdate] (utxosWidget model)
            `nodeVisible` (model ^. #homeModel % #scene == HomeUTxOs)
        , box_ [mergeRequired reqUpdate] (nativeAssetsWidget model)
            `nodeVisible` (model ^. #homeModel % #scene == HomeAssets)
        ] 

    hasPaymentWallets :: Bool
    hasPaymentWallets = model ^. #knownWallets % #paymentWallets /= []

    isAdding :: Bool
    isAdding = model ^. #homeModel % #addingWallet

    isEditing :: Bool
    isEditing = model ^. #homeModel % #editingWallet

    isDeleting :: Bool
    isDeleting = model ^. #homeModel % #deletingWallet

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
            -- The widget is initialized using PairPaymentWallet. It is the same for all wallet 
            -- types.
            , box (mainButton "Add Wallet" $ HomeEvent $ PairPaymentWallet StartAdding) 
                `styleBasic` [padding 20]
            ]
        ] `nodeVisible` (not isAdding)

morePopup :: AppModel -> AppNode
morePopup _ = do
  vstack
    [ tooltip_ "More" [tooltipDelay 0] $
        button horizontalMoreIcon (HomeEvent ShowMorePopup)
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
    , customPopup (toLensVL $ #homeModel % #showMorePopup) $
        vstack
          [ button "Add Wallet" (HomeEvent $ PairPaymentWallet StartAdding)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , button "Edit Name" (HomeEvent $ ChangePaymentWalletName StartAdding)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , separatorLine `styleBasic` [fgColor black, padding 5]
          , button "Delete Wallet" (HomeEvent $ DeletePaymentWallet GetDeleteConfirmation)
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

editPaymentWalletWidget :: AppModel -> AppNode
editPaymentWalletWidget _ = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Wallet Name:"
            , spacer
            , textField (toLensVL $ #extraTextField) 
                `styleBasic` [width 500]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , mainButton "Confirm" $ HomeEvent $ ChangePaymentWalletName ConfirmAdding
        , spacer
        , button "Cancel" $ HomeEvent $ ChangePaymentWalletName CancelAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]

confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget model = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , model ^. #homeModel % #selectedWallet % #alias
        , "'?"
        ]
    , hstack 
        [ filler
        , mainButton "Confirm" $ HomeEvent $ DeletePaymentWallet ConfirmDeletion
        , spacer
        , button "Cancel" $ HomeEvent $ DeletePaymentWallet CancelDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]

