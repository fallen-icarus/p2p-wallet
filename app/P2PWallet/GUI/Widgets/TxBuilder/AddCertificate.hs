module P2PWallet.GUI.Widgets.TxBuilder.AddCertificate where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core.PoolID
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

addCertificateWidget :: AppWenv -> AppModel -> AppNode
addCertificateWidget wenv model = 
  let firstStakeWallet = maybeHead $ model ^. wallets . stakeWallets
      boolLens' = 
        boolLens (fromMaybe def firstStakeWallet)
                 (txBuilderModel . newCertificate . _2 . internalWallet)
      walletLens = 
        maybeLens (fromMaybe def firstStakeWallet)
                  (txBuilderModel . newCertificate . _2 . internalWallet)

      unWrapper (Delegation (PoolID poolId')) = poolId'
      unWrapper _ = ""

      certificateLens = 
        sumsOfProductsLens 
          unWrapper
          (Delegation . PoolID)
          (txBuilderModel . newCertificate . _2 . certificateAction)

      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = vstack_ [childSpacing]
        [ -- Only allow toggling to `internalWallet` if there are tracked stake wallets.
          widgetIf (isJust firstStakeWallet) $ hstack_ [childSpacing]
            [ label "Output To:"
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
            , textField (txBuilderModel . newCertificate . _2 . stakeAddress)
                `styleBasic` [width 600]
            ]
        , hstack_ [childSpacing]
            [ label "Action:"
            , hstack_ [childSpacing]
                [ labeledRadio "Registration" Registration $ 
                    txBuilderModel . newCertificate . _2 . certificateAction
                , spacer
                , labeledRadio "Deregistration" Deregistration $ 
                    txBuilderModel . newCertificate . _2 . certificateAction
                , spacer
                , labeledRadio "Delegation" (Delegation "") $
                    txBuilderModel . newCertificate . _2 . certificateAction
                ]
            ]
        , widgetIf (isDelegation $ model ^. txBuilderModel . newCertificate . _2 . certificateAction) $ 
            hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "Target Pool ID:"
              , spacer
              , textField certificateLens
                  `styleBasic` [width 600]
              ]
        ]

  in centerWidget $ vstack 
       [ editFields
       , spacer
       , hstack 
           [ filler
           , mainButton "Confirm" $ TxBuilderEvent InsertNewCertificate 
           , spacer
           , button "Cancel" $ TxBuilderEvent $ ChangeBuilderScene BuilderSummary
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
