module P2PWallet.GUI.Widgets.TxBuilder.AddWithdrawal where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

addWithdrawalWidget :: AppWenv -> AppModel -> AppNode
addWithdrawalWidget wenv model = 
  let firstStakeWallet = maybeHead $ model ^. wallets . stakeWallets
      boolLens' = 
        boolLens (fromMaybe def firstStakeWallet)
                 (txBuilderModel . newWithdrawal . _2 . internalWallet)
      walletLens = 
        maybeLens (fromMaybe def firstStakeWallet)
                  (txBuilderModel . newWithdrawal . _2 . internalWallet)

      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = vstack_ [childSpacing]
        [ -- Only allow toggling to `internalWallet` if there are tracked stake wallets.
          widgetIf (isJust firstStakeWallet) $ hstack_ [childSpacing]
            [ label "Withdrawal For:"
            , hgrid_ [childSpacing] 
                [ labeledRadio "Self" True boolLens'
                , labeledRadio "Other" False boolLens'
                ]
            ]
        , widgetIf (model ^# boolLens') $ hstack
            [ label "Stake Address:"
            , spacer
            , textDropdown_ 
                  walletLens 
                  (model ^. wallets . stakeWallets) 
                  (view alias) 
                  []
                `styleBasic` [width 200]
            ]
        , widgetIf (not $ model ^# boolLens') $ hstack
            [ label "Stake Address:"
            , spacer
            , textField (txBuilderModel . newWithdrawal . _2 . stakeAddress)
                `styleBasic` [width 600]
            ]
        , hstack
            [ label "ADA:"
            , spacer
            , numericField_ (txBuilderModel . newWithdrawal . _2 . rawAdas) [decimals 6]
                `styleBasic` [width 200]
            ]
        ]

  in centerWidget $ vstack 
       [ editFields
       , spacer
       , hstack 
           [ filler
           , mainButton "Confirm" $ TxBuilderEvent InsertNewWithdrawal
           , spacer
           , button "Cancel" $ TxBuilderEvent $ ChangeBuilderScene BuilderSummary
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
