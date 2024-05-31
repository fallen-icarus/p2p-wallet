{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.Transactions where

import Monomer
import Prettyprinter (align, pretty, vsep)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Transaction
import P2PWallet.Data.Wallets hiding (toggleDetails)
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Information
import P2PWallet.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | Calculate the net ADA flux from this address in the transaction.
txValueFromWallet :: PaymentAddress -> Transaction -> Ada
txValueFromWallet addr tx = 
  let isFromAddress x = x ^. #paymentAddress == addr
      collateralReturned =
        maybe 0 (\o -> if isFromAddress o then toAda $ o ^. #lovelace else 0) (tx ^. #collateralOutput)
      spent = 
          sum (map (toAda . view #lovelace) $ filter isFromAddress $ tx ^. #inputs)
        + sum (map (toAda . view #lovelace) $ filter isFromAddress $ tx ^. #collateralInputs)
      received = 
          sum (map (toAda . view #lovelace) $ filter isFromAddress $ tx ^. #outputs)
        + collateralReturned
  in received - spent

transactionsWidget :: AppModel -> AppNode
transactionsWidget model =
    cushionWidgetH $ vstack
      [ flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
          vstack_ [childSpacing] (map txRow sample)
            `styleBasic` [padding 10]
      , filler
      ]
  where
    wallet :: PaymentWallet
    wallet = model ^. #homeModel % #selectedWallet

    sample :: [Transaction]
    sample = wallet ^. #transactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let txValueFlux = txValueFromWallet (wallet ^. #paymentAddress) tx
      let valueColor
            | txValueFlux >= 0 = customBlue
            | otherwise = customRed
      vstack
        [ hstack 
            [ copyableLabelMain txHash
                `styleBasic` [textSize 12]
            , spacer_ [width 2]
            , tooltip_ "Inspect" [tooltipDelay 0] $
                button remixSearchLine (HomeEvent $ InspectHomeTransaction tx)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , paddingL 2
                    , paddingR 2
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label (fromString $ printf "%D ADA" txValueFlux)
                `styleBasic` 
                  [ textSize 12
                  , textColor valueColor
                  ]
            ]
        , hstack
            [ label remixCalendarLine
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (model ^. #config % #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer
            , label remixTimeLine
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalTime (model ^. #config % #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            , label (unwords ["Fee:", fromString $ printf "%D ADA" $ toAda fee])
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
           ] 
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

inspectionWidget :: Transaction -> AppModel -> AppNode
inspectionWidget Transaction{..} model = do
    vstack
      [ centerWidget $ vscroll_ [wheelRate 100] $ vstack_ [childSpacing_ 5]
          [ centerWidgetH $ label "Transaction Summary" 
              `styleBasic` 
                [ textFont "Italics"
                , paddingB 10
                ]
          , copyableLabelDetail "Tx Hash:" txHash
          , copyableLabelDetail "Block Time:" $ show blockTime
          , copyableLabelDetail "Block Height:" $ show blockHeight
          , copyableLabelDetail "Fee:" $ fromString $ printf "%D ADA" $ toAda fee
          , hstack
              [ copyableLabelDetail "Deposit:" $ fromString $ printf "%D ADA" $ toAda deposit
              , mainButton remixInformationLine (Alert depositSignMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , paddingT 0
                    , paddingB 0
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , copyableLabelDetail "Invalid Before:" $ 
              maybe "none" (fromString . printf "slot %s") invalidBefore
          , copyableLabelDetail "Invalid After:" $ 
              maybe "none" (fromString . printf "slot %s") invalidAfter
          , utxoField "Reference Inputs:" 
              senderSymbol
              #showReferenceInputs 
              #referenceInputs 
              referenceInputs
          , utxoField "Collateral Inputs:" 
              senderSymbol
              #showCollateralInputs 
              #collateralInputs 
              collateralInputs
          , utxoField "Collateral output:" 
              receiverSymbol
              #showCollateralOutput 
              collateralOutputLens
              (maybe [] (:[]) collateralOutput)
          , utxoField "Inputs:" 
              senderSymbol
              #showInputs 
              #inputs 
              inputs
          , utxoField "Outputs:" 
              receiverSymbol
              #showOutputs 
              #outputs 
              outputs
          , filler
          , hstack
              [ filler
              , button "Close" $ HomeEvent CloseInspectedHomeTransaction
              ]
          ] `styleBasic`
              [ bgColor customGray3
              , padding 30
              , radius 10
              ]
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , paddingT 30
          , paddingB 30
          , paddingL 30
          , paddingR 30
          , radius 10
          ]
  where
    senderSymbol :: AppNode
    senderSymbol = tooltip_ "From this wallet" [tooltipDelay 0] $ label remixUserSharedLine
      `styleBasic` [textSize 10, padding 0, textMiddle, textColor customRed, textFont "Remix"]

    receiverSymbol :: AppNode
    receiverSymbol = tooltip_ "To this wallet" [tooltipDelay 0] $ label remixUserReceivedLine
      `styleBasic` [textSize 10, padding 0, textMiddle, textColor customBlue, textFont "Remix"]

    moreIcon :: Bool -> Text
    moreIcon detailsOpen
      | detailsOpen = remixCloseCircleLine
      | otherwise = remixMoreLine

    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    moreOffStyle :: Style
    moreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    txMoreOffStyle :: Style
    txMoreOffStyle = 
      def `styleBasic` 
            [ bgColor black
            , textColor customBlue
            , radius 20
            , paddingT 2
            , paddingB 2
            , paddingR 5
            , paddingL 5
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoField 
      :: Text
      -> AppNode
      -> Lens' Transaction Bool 
      -> Lens' Transaction [TransactionUTxO] 
      -> [TransactionUTxO] 
      -> AppNode
    utxoField caption userSymbol finalLens utxoLens utxos =
      let showRoot = #homeModel % #inspectedTransaction in
      if null utxos then
        hstack
          [ label caption
              `styleBasic`
                [ padding 0
                , radius 5
                , textMiddle
                , textSize 12
                , border 0 transparent
                , textColor customBlue
                , bgColor transparent
                ]
          , spacer
          , label "none" `styleBasic` [textColor lightGray, textSize 12]
          ]
      else
        vstack
          [ hstack
              [ label caption
                  `styleBasic`
                    [ padding 0
                    , radius 5
                    , textMiddle
                    , textSize 12
                    , border 0 transparent
                    , textColor customBlue
                    , bgColor transparent
                    ]
              , spacer
              , toggleButton_ remixMoreLine (toLensVL $ showRoot % toggleShow finalLens)
                  [toggleButtonOffStyle txMoreOffStyle]
                  `styleBasic` 
                    [ textSize 10
                    , textColor customRed
                    , textFont "Remix"
                    , textMiddle
                    , radius 20
                    , paddingT 2
                    , paddingB 2
                    , paddingR 5
                    , paddingL 5
                    , bgColor black
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , widgetIf (model ^. showRoot % toggleShow finalLens) $
              flip styleBasic [padding 5] $
                vstack_ [childSpacing] (map (utxoRow userSymbol utxoLens) utxos)
                  `styleBasic` [padding 10]
          ]

    utxoRow :: AppNode -> Lens' Transaction [TransactionUTxO] -> TransactionUTxO -> AppNode
    utxoRow userSymbol finalLens u@TransactionUTxO{..} =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf (showTxOutRef utxoRef)
                    `styleBasic` [textSize 12]
                , filler
                , label (fromString $ printf "%D ADA" $ toAda lovelace) 
                    `styleBasic` [textSize 12]
                ]
            , hstack
                [ label remixCalendarLine
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalDate (model ^. #config % #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , label remixTimeLine
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalTime (model ^. #config % #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , widgetIf (not $ null nativeAssets) $ 
                    tooltip_ "Native Assets" [tooltipDelay 0] $ label remixCoinsLine
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , widgetIf (not $ null nativeAssets) $ spacer_ [width 3]
                , widgetIf (isJust datumHash) $ 
                    tooltip_ "Datum" [tooltipDelay 0] $ label datumIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , widgetIf (isJust datumHash) $ spacer_ [width 3]
                , widgetIf (isJust referenceScriptHash) $ 
                    tooltip_ "Reference Script" [tooltipDelay 0] $ label scriptIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , widgetIf 
                    (paymentAddress == model ^. #homeModel % #selectedWallet % #paymentAddress)
                    userSymbol
                , spacer_ [width 3]
                , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                    toggleButton_ (moreIcon showDetails)
                      (toLensVL $ #homeModel 
                                % #inspectedTransaction 
                                % toggleShow (finalLens % toggleDetails utxoRef))
                      [toggleButtonOffStyle moreOffStyle]
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        , padding 0
                        , bgColor transparent
                        , border 0 transparent
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
               ] 
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , widgetIf showDetails $ utxoDetails u -- `nodeVisible` showDetails
        ]

    datumIcon :: Text
    datumIcon = toGlyph 0XF2F5

    scriptIcon :: Text
    scriptIcon = toGlyph 0XF433

    utxoDetails :: TransactionUTxO -> AppNode
    utxoDetails TransactionUTxO{..} = 
      hstack
        [ filler
        , vstack
            [ copyableLabelFor_ "Payment Address:" (toText paymentAddress) 10
                `styleBasic` [padding 2]
            , copyableLabelFor_ "Stake Address:" (maybe "none" toText stakeAddress) 10
                `styleBasic` [padding 2]
            , widgetMaybe referenceScriptHash $ \hash ->
                copyableLabelFor_ "Reference Script Hash:" hash 10
                  `styleBasic` [padding 2]
            , widgetMaybe datumHash $ \hash ->
                copyableLabelFor_ "Datum Hash:" hash 10
                  `styleBasic` [padding 2]
            , widgetMaybe inlineDatum $ \x ->
                copyableLabelFor_ "Inline Datum:" (showValue x) 10
                  `styleBasic` [padding 2]
            , widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 10, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , copyableTextArea (show $ align $ vsep $ map pretty nativeAssets)
                          `styleBasic` [textSize 10, textColor lightGray, maxWidth 700]
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]


-- | A label button that will copy itself.
copyableLabelMain :: Text -> WidgetNode s AppEvent
copyableLabelMain caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 12
      , border 0 transparent
      , textColor white
      , bgColor transparent
      , textFont "Italics"
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy other data.
copyableLabelDetail :: Text -> Text -> WidgetNode s AppEvent
copyableLabelDetail caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ 
        button caption (CopyText info)
          `styleBasic`
            [ padding 0
            , radius 5
            , textMiddle
            , textSize 12
            , border 0 transparent
            , textColor customBlue
            , bgColor transparent
            ]
          `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label info 
        `styleBasic` 
          [ textColor lightGray
          , textSize 12
          ]
    ]

