module P2PWallet.GUI.Widgets.Home where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Home.About
import P2PWallet.GUI.Widgets.Home.Assets
import P2PWallet.GUI.Widgets.Home.Details
import P2PWallet.GUI.Widgets.Home.PairPaymentWallet
import P2PWallet.GUI.Widgets.Home.Transactions
import P2PWallet.GUI.Widgets.Home.UTxOs
import P2PWallet.GUI.Widgets.Home.WatchPaymentWallet
import P2PWallet.Prelude

homeWidget :: AppWenv -> AppModel -> AppNode
homeWidget wenv model = do
    zstack
      [ mainWidget `nodeVisible` (hasPaymentWallets && noDetails && not isAdding)
      , addFirstWalletWidget `nodeVisible` (noDetails && not isAdding && not hasPaymentWallets)
      , widgetIf isPairing $ centerWidget $
          box_ [alignCenter] $ vstack
            [ hstack [ filler, label "Pairing", filler ]
            , spacer
            , pairPaymentWidget wenv model
            ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , widgetIf isAddingWatched $ centerWidget $
          box_ [alignCenter] $ vstack
            [ hstack [ filler, label "New Watched", filler ]
            , spacer
            , watchPaymentWidget wenv model
            ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , detailsOverlay model `nodeVisible` (not noDetails)
      , widgetIf (model ^. homeModel . filteringAssets) $ flip styleBasic [bgColor filterFade] $ 
          centerWidget $
            box_ [alignCenter] $ vstack
              [ hstack [filler, label "Asset Filter Settings", filler]
              , spacer
              , assetFilterInfo wenv model
              ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , widgetIf (model ^. homeModel . filteringTxs) $ flip styleBasic [bgColor filterFade] $
          centerWidget $
            box_ [alignCenter] $ vstack
              [ hstack [filler, label "Transaction Filter Settings", filler]
              , spacer
              , txFilterInfo wenv model
              ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , widgetIf (model ^. homeModel . filteringUtxos) $ flip styleBasic [bgColor filterFade] $
          centerWidget $
            box_ [alignCenter] $ vstack
              [ hstack [filler, label "UTxO Filter Settings", filler]
              , spacer
              , utxoFilterInfo wenv model
              ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      ]

  where
    sectionBg :: Color
    sectionBg = wenv ^. L.theme . L.sectionColor

    filterFade :: Color
    filterFade = darkGray & L.a .~ 0.8

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old new 
      | old ^. wallets . paymentWallets /= new ^. wallets . paymentWallets = True
      | old ^. homeModel . selectedWallet /= new ^. homeModel . selectedWallet = True
      | otherwise = False

    hasPaymentWallets :: Bool
    hasPaymentWallets = model ^. wallets . paymentWallets /= []

    noDetails :: Bool
    noDetails = isNothing $ model ^. homeModel . details

    isPairing :: Bool
    isPairing = model ^. homeModel . pairing

    isAddingWatched :: Bool
    isAddingWatched = model ^. homeModel . watching

    isAdding :: Bool
    isAdding = isPairing || isAddingWatched

    isWatched :: Bool
    isWatched = isNothing $ model ^. homeModel . selectedWallet . paymentKeyPath

    walletTypeIcon :: Text
    walletTypeIcon
      | isWatched = remixEyeLine
      | otherwise = remixLinksLine

    -- The main widget that should only be shown if there are currently tracked payment wallets
    -- AND no details need to be shown.
    mainWidget :: AppNode
    mainWidget =
      vstack
        [ spacer
        , hstack_ [childSpacing] 
            [ filler
            , label $ toText @String $ printf "UTxO Balance: %D ADA" $ toADA $
                model ^. homeModel . selectedWallet . lovelaces
            , separatorLine
            , textDropdown_ 
                  (homeModel . selectedWallet) 
                  (model ^. wallets . paymentWallets) 
                  (view alias) 
                  []
                `styleBasic` [width 200]
            , label walletTypeIcon 
                `styleBasic` [textFont "Remix",paddingT 15]
            , separatorLine
            , customButton wenv "Refresh" remixRefreshLine (SyncWallets StartSync)
            , filler
            ] 
        , spacer
        , hgrid_ [childSpacing] 
            [ button "About" $ HomeEvent $ ChangeHomeScene HomeAbout
            , button "Transactions" $ HomeEvent $ ChangeHomeScene HomeTransactions
            , button "UTxOs" $ HomeEvent $ ChangeHomeScene HomeUTxOs
            , button "Assets" $ HomeEvent $ ChangeHomeScene HomeAssets
            ]
        , spacer
        , box_ [mergeRequired reqUpdate] (aboutWidget wenv model)
            `nodeVisible` (model ^. homeModel . scene == HomeAbout)
        , box_ [mergeRequired reqUpdate] (utxoWidget wenv model)
            `nodeVisible` (model ^. homeModel . scene == HomeUTxOs)
        , box_ [mergeRequired reqUpdate] (assetWidget wenv model)
            `nodeVisible` (model ^. homeModel . scene == HomeAssets)
        , box_ [mergeRequired reqUpdate] (txWidget wenv model)
            `nodeVisible` (model ^. homeModel . scene == HomeTransactions)
        ] 

    -- A welcome message when there are currently no tracked payment wallets.
    addFirstWalletWidget :: AppNode
    addFirstWalletWidget =
      vstack
        [ centerWidget $
            flip styleBasic [bgColor sectionBg, padding 20, radius 5] $ 
              box $ 
                label "Add your first payment wallet to begin!" 
                 `styleBasic` [textFont "Italics"]
        , filler
        , hstack
            [ filler
            , box (mainButton "Pair" $ HomeEvent $ PairPaymentWallet StartAdding) 
                `styleBasic` [paddingR 5, paddingB 20, paddingL 20, paddingT 20]
            , box (mainButton "Watch" $ HomeEvent $ WatchPaymentWallet StartAdding) 
                `styleBasic` [paddingL 5, paddingB 20, paddingR 20, paddingT 20]
            ]
        ] `nodeVisible` (not isAdding)
