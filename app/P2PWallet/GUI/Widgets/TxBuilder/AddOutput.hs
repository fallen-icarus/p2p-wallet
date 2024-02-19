module P2PWallet.GUI.Widgets.TxBuilder.AddOutput where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

addOutputWidget :: AppWenv -> AppModel -> AppNode
addOutputWidget wenv model = 
  let firstPaymentWallet = maybeHead $ model ^. wallets . paymentWallets
      rootLens = txBuilderModel . newOutput . _2
      boolLens' = 
        boolLens (fromMaybe def firstPaymentWallet) 
                 (rootLens . internalWallet)
      walletLens = 
        maybeLens (fromMaybe def firstPaymentWallet)
                  (rootLens . internalWallet)
      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = vstack_ [childSpacing]
        [ -- Only allow toggling to `internalWallet` if there are tracked payment wallets.
          widgetIf (isJust firstPaymentWallet) $ hstack_ [childSpacing]
            [ label "Output To:"
            , hgrid_ [childSpacing] 
                [ labeledRadio "Self" True boolLens'
                , labeledRadio "Other" False boolLens'
                ]
            ]
        , widgetIf (model ^# boolLens') $ hstack
            [ label "Wallet:"
            , spacer
            , textDropdown_ walletLens (model ^. wallets . paymentWallets) (view alias) []
                `styleBasic` [width 200]
            ]
        , widgetIf (not $ model ^# boolLens') $ hstack
            [ label "Address:"
            , spacer
            , textField (txBuilderModel . newOutput . _2 . paymentAddress)
            ]
        , hstack
            [ label "ADA:"
            , spacer
            , numericField_ (txBuilderModel . newOutput . _2 . rawAdas) [decimals 6]
                `styleBasic` [width 200]
            ]
        , label "Native Assets (separated with newlines):"
        , textArea (txBuilderModel . newOutput . _2 . nativeAssets)
            `styleBasic` [height 180]
        ]

  in centerWidget $ vstack 
       [ editFields
       , spacer
       , hstack 
           [ filler
           , mainButton "Confirm" $ TxBuilderEvent InsertNewOutput 
           , spacer
           , button "Cancel" $ TxBuilderEvent $ ChangeBuilderScene BuilderSummary
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
