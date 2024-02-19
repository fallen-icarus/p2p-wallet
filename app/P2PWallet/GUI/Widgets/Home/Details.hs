module P2PWallet.GUI.Widgets.Home.Details 
  ( detailsOverlay
  ) where

import Monomer
import Prettyprinter
import Data.Maybe (fromJust)

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- A close button in the lower right hand corner of the details page.
detailCloseButton :: WidgetNode s AppEvent
detailCloseButton =
  vstack_ [childSpacing]
    [ filler
    , hstack 
        [ filler
        , button "Close" $ HomeEvent CloseHomeDetails 
        , spacer
        , spacer
        ]
    , spacer
    ]

detailTitle :: Text -> WidgetNode s AppEvent
detailTitle title =
  hstack
    [ filler
    , label title `styleBasic` [textSize 20, textFont "Medium"]
    , filler
    ]

detailTextField :: Text -> Text -> WidgetNode s AppEvent
detailTextField l f =
  hstack
    [ spacer
    , spacer
    , label l 
    , copyableTextField f
    ] `styleBasic` [paddingT 0, paddingB 0]

detailTextArea 
  :: AppModel 
  -> Text 
  -> ALens' ExpandedFields Bool 
  -> Text 
  -> WidgetNode AppModel AppEvent
detailTextArea model l aLens f = do
  vstack_ [childSpacing]
    [ hstack
        [ spacer
        , spacer
        , label l
        , spacer
        , toggleButton remixMoreLine (homeModel . expandedFields . aLens)
            `styleHover` [cursorIcon CursorHand]
            `styleBasic` 
              [ textFont "Remix"
              , padding 0
              , textMiddle
              , border 0 transparent
              ]
        ]
    , hstack
        [ spacer
        , spacer
        , hstack
            [ spacer
            , copyableTextArea f
            ]
        ] `nodeVisible` (model ^# homeModel . expandedFields . aLens)
    ] `styleBasic` [padding 0]

detailsOverlay :: AppModel -> WidgetNode AppModel AppEvent
detailsOverlay model =
    box $ vscroll_ [wheelRate 50] $ 
      case model ^. homeModel . details of
        Nothing -> copyableTextArea ""
        Just (HomeUTxO utxo) -> 
          vstack_ [childSpacing] 
            [ spacer
            , detailTitle "UTxO Details"
            , spacer
            , detailTextField "Output Reference:" $ showTxOutRef $ utxo ^. utxoRef
            , detailTextField "Time Stamp:" $ showLocalTime "%D %T %Z" $ utxo ^. blockTime
            , detailTextField "Value:" $ fromString $ printf "%D ADA" $ 
                toADA $ utxo ^. lovelaces
            , detailTextField "Reference Script Hash:" $ fromMaybe "none" $ 
                utxo ^. referenceScriptHash
            , detailTextField "Datum Hash:" $ fromMaybe "none" $ utxo ^. datumHash
            , if isNothing $ utxo ^. inlineDatum then 
                detailTextField "Inline Datum:" "none"
              else 
                vstack_ [childSpacing]
                  [ hstack
                      [ spacer
                      , spacer
                      , label "Inline Datum:"
                      ]
                  , hstack
                      [ spacer
                      , spacer
                      , copyableTextArea $ maybe "none" showValue $ utxo ^. inlineDatum
                      ]
                  ]
            , if null $ utxo ^. nativeAssets then 
                detailTextField "Natve Assets:" "none"
              else 
                vstack_ [childSpacing]
                  [ hstack
                      [ spacer
                      , spacer
                      , label "Native Assets:" 
                      ]
                  , hstack
                      [ spacer
                      , spacer
                      , copyableTextArea $ show $ 
                          indent 4 $ align $ vsep $ map pretty $ utxo ^. nativeAssets
                      ]
                  ]
            , detailCloseButton
            ]
        Just (HomeTransaction tx) -> 
          vstack_ [childSpacing]
            [ spacer
            , detailTitle "Transaction Details"
            , spacer
            , detailTextField "Tx Hash:" $ tx ^. txHash
            , detailTextField "Time Stamp:" $ showLocalTime "%D %T %Z" $ tx ^. blockTime
            , detailTextField "Fee:" $ fromString $ printf "%D ADA" $ toADA $ tx ^. fee
            , detailTextField "Tx Size:" $ fromString $ printf "%d bytes" $ tx ^. size
            , detailTextField "Deposit:" $ fromString $ printf "%D ADA" $ toADA $ tx ^. deposit
            , detailTextField "Invalid Before:" $ 
                maybe "none" (fromString . printf "slot %s") $ tx ^. invalidBefore
            , detailTextField "Invalid After:" $ 
                maybe "none" (fromString . printf "slot %s") $ tx ^. invalidAfter
            , if null $ tx ^. referenceInputs then 
                detailTextField "Reference Inputs:" "none"
              else 
                detailTextArea model "Reference Inputs:" referenceInputs $ 
                  show $ vsep $ map pretty $ tx ^. referenceInputs
            , if null $ tx ^. collateralInputs then 
                detailTextField "Collateral Inputs:" "none"
              else 
                detailTextArea model "Collateral Inputs:" collateralInputs $ 
                  show $ vsep $ map pretty $ tx ^. collateralInputs
            , maybe 
                (detailTextField "Collateral Output:" "none") 
                (detailTextArea model "Collateral Output:" collateralOutput . show . pretty)
                (tx ^. collateralOutput)
            , if null $ tx ^. certificates then 
                detailTextField "Certificates:" "none"
              else 
                detailTextArea model "Certificates:" certificates $ 
                  show $ vsep $ intersperse mempty $ map pretty $ tx ^. certificates
            , if null $ tx ^. withdrawals then 
                detailTextField "Withdrawals:" "none"
              else 
                detailTextArea model "Withdrawals:" withdrawals $ 
                  show $ vsep $ intersperse mempty $ map pretty $ tx ^. withdrawals
            , detailTextArea model "Inputs:" inputs $ show $ 
                vsep $ intersperse mempty $ map pretty $ tx ^. inputs
            , detailTextArea model "Outputs:" outputs $ show $ 
                vsep $ intersperse mempty $ map pretty $ tx ^. outputs
            , detailCloseButton
            ]
        Just (HomeAsset asset@NativeAsset{_fingerprint}) -> do
          let -- Show all utxos with this asset.
              sample = filter (hasAssetWithFingerprint _fingerprint) $ 
                model ^. homeModel . selectedWallet . utxos
              row u = hstack
                [ spacer
                , spacer
                , copyableTextField $ showTxOutRef $ u ^. utxoRef
                , filler
                , label $ show $ view quantity $ fromJust $ 
                    find (\a -> a ^. fingerprint == _fingerprint) $ u ^. nativeAssets
                , spacer
                , spacer
                ]
          vstack_ [childSpacing]
            [ spacer
            , detailTitle "Asset Info"
            , label "Name" `styleBasic` [padding 10]
            , detailTextField "Policy ID:" (asset ^. policyId)
            , detailTextField "Token Name:" (asset ^. tokenName)
            , detailTextField "Fingerprint:" (asset ^. fingerprint)
            , label ("UTxOs (" <> show (length sample) <> ")") `styleBasic` [padding 10]
            , vstack $ map row sample
            , spacer
            , detailCloseButton
            ]
