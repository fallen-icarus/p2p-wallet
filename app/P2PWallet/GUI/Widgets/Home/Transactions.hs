module P2PWallet.GUI.Widgets.Home.Transactions 
  ( 
    transactionsWidget
  , inspectionWidget
  ) where

import Monomer
import Prettyprinter (align, pretty, vsep)
import Data.Text qualified as Text
import Data.Map qualified as Map
import Data.Time.Format qualified as Time

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.PaymentWallet
import P2PWallet.Data.Core.Transaction
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{homeModel=HomeModel{..},config,reverseTickerMap} =
    zstack
      [ cushionWidgetH $ vstack
          [ centerWidgetH $ hstack
              [ label shownDateRange
                  `styleBasic` 
                    [ padding 0
                    , textMiddle
                    , textFont "Italics"
                    ]
              , spacer_ [width 5]
              , tooltip_ "Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #homeModel % #showTransactionFilter)
                    [toggleButtonOffStyle toggleOffStyle]
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , paddingT 0
                      , paddingB 0
                      , paddingL 5
                      , paddingR 5
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , widgetIf (sample /= []) $ flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map txRow sample)
                `styleBasic` [padding 10]
          , widgetIf (null sample) $ 
              centerWidget $
                label "No transactions found."
                 `styleBasic` [textFont "Italics"]
          , filler
          ]
      , txFilterWidget model `nodeVisible` (model ^. #homeModel % #showTransactionFilter)
      ]
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    timeZone :: TimeZone
    timeZone = config ^. #timeZone

    today :: Day
    today = config ^. #currentDay

    (mLowerDay,mUpperDay) = txFilterModel ^. #dateRange

    startTime :: POSIXTime
    startTime = maybe 0 (localTimeToPosixTime timeZone . beginningOfDay) mLowerDay

    endTime :: POSIXTime
    endTime = localTimeToPosixTime timeZone 
            $ endOfDay
            $ fromMaybe today mUpperDay

    shownDateRange :: Text
    shownDateRange = unwords
      [ maybe "Beginning" (const $ showLocalDate timeZone startTime) mLowerDay 
      , "-"
      , maybe "Present" (const $ showLocalDate timeZone endTime) mUpperDay 
      ]


    withinDateRange :: Transaction -> Bool
    withinDateRange Transaction{blockTime} = do
      blockTime >= startTime && blockTime <= endTime

    searchTargets :: [Text]
    searchTargets = words $ replace "," " " $ txFilterModel ^. #search

    sample :: [Transaction]
    sample = applySearchFilter selectedWallet reverseTickerMap searchTargets 
           . filter withinDateRange 
           $ selectedWallet ^. #transactions

    txAssetFluxWidget :: NativeAsset -> AppNode
    txAssetFluxWidget NativeAsset{..} = do
      let (fluxIcon,color)
            | quantity < 0 = (upArrowIcon, customRed)
            | otherwise = (downArrowIcon, customBlue)
          (name,formattedQuantity) = case Map.lookup (policyId,tokenName) reverseTickerMap of
            Nothing -> (display fingerprint, show quantity)
            Just (tckr,decimal) -> (display tckr, show $ formatQuantity decimal quantity)
      hstack_ [childSpacing_ 3]
        [ label fluxIcon 
            `styleBasic` 
              [ textFont "Remix"
              , textSize 8
              , bgColor color
              , padding 1
              , radius 20
              , textMiddle
              ]
        , copyableLabelSelf name 8 lightGray
        , label formattedQuantity
            `styleBasic` 
              [ textSize 8, padding 3, radius 3, bgColor customGray3, textColor color]
        ] `styleBasic` [bgColor customGray4, paddingT 2, paddingB 2, paddingL 2, paddingR 0]

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let (adaFlux,assetFlux) = txValueFromWallet (selectedWallet ^. #paymentAddress) tx
          adaValueColor
            | adaFlux >= 0 = customBlue
            | otherwise = customRed
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 white
            , spacer_ [width 2]
            , tooltip_ "Inspect" [tooltipDelay 0] $
                button inspectIcon (HomeEvent $ InspectHomeTransaction tx)
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
            , label (display adaFlux)
                `styleBasic` 
                  [ textSize 10
                  , textColor adaValueColor
                  ]
            ]
        , hstack
            [ label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer
            , label clockIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalTime (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            , label (unwords ["Fee:", display fee])
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
           ] 
        , widgetIf (not $ null assetFlux) $ vstack_ [childSpacing_ 3] $ 
            for (groupInto 3 assetFlux) $ \assetRow -> 
              hstack_ [childSpacing_ 3] $ [filler] <> map txAssetFluxWidget assetRow
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

inspectionWidget :: Transaction -> AppModel -> AppNode
inspectionWidget Transaction{..} AppModel{homeModel=HomeModel{..},reverseTickerMap} = do
    vstack
      [ vstack
          [ centerWidgetH $ label "Transaction Summary" 
              `styleBasic` 
                [ textFont "Italics"
                , paddingB 10
                ]
          , cushionWidget $ vscroll_ [wheelRate 100] $ vstack_ [childSpacing_ 5]
              [ copyableLabelFor 10 "Tx Hash:" txHash
              , copyableLabelFor 10 "Block Time:" $ show blockTime
              , copyableLabelFor 10 "Block Height:" $ show blockHeight
              , copyableLabelFor 10 "Fee:" $ fromString $ printf "%D ADA" $ toAda fee
              , hstack
                  [ copyableLabelFor 10 "Deposit:" $ fromString $ printf "%D ADA" $ toAda deposit
                  , mainButton helpIcon (Alert depositSignMsg)
                      `styleBasic`
                        [ border 0 transparent
                        , radius 20
                        , paddingT 0
                        , paddingB 0
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , textSize 10
                        ]
                      `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                  ]
              , copyableLabelFor 10 "Invalid Before:" $ 
                  maybe "none" (fromString . printf "slot %s") invalidBefore
              , copyableLabelFor 10 "Invalid After:" $ 
                  maybe "none" (fromString . printf "slot %s") invalidAfter
              , certificateField inspectedTransaction
              , withdrawalField inspectedTransaction
              , utxoField "Reference Inputs:" 
                  Nothing
                  #showReferenceInputs 
                  #referenceInputs 
                  referenceInputs
              , utxoField "Collateral Inputs:" 
                  Nothing
                  #showCollateralInputs 
                  #collateralInputs 
                  collateralInputs
              , utxoField "Inputs:" 
                  (Just senderSymbol)
                  #showInputs 
                  #inputs 
                  inputs
              , utxoField "Outputs:" 
                  (Just receiverSymbol)
                  #showOutputs 
                  #outputs 
                  outputs
              ] 
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
          , padding 30
          , radius 10
          ]
  where
    senderSymbol :: AppNode
    senderSymbol = tooltip_ "From this wallet" [tooltipDelay 0] $ label userSharedIcon
      `styleBasic` [textSize 10, padding 0, textMiddle, textColor customRed, textFont "Remix"]

    receiverSymbol :: AppNode
    receiverSymbol = tooltip_ "To this wallet" [tooltipDelay 0] $ label userReceivedIcon
      `styleBasic` [textSize 10, padding 0, textMiddle, textColor customBlue, textFont "Remix"]

    moreIcon :: Bool -> Text
    moreIcon detailsOpen
      | detailsOpen = closeCircleIcon
      | otherwise = horizontalMoreIcon

    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    specificUtxoMoreOffStyle :: Style
    specificUtxoMoreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoField 
      :: Text
      -> Maybe AppNode
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
                , textSize 10
                , border 0 transparent
                , textColor customBlue
                , bgColor transparent
                ]
          , spacer
          , label "none" `styleBasic` [textColor lightGray, textSize 10]
          ]
      else
        vstack
          [ hstack
              [ label caption
                  `styleBasic`
                    [ padding 0
                    , radius 5
                    , textMiddle
                    , textSize 10
                    , border 0 transparent
                    , textColor customBlue
                    , bgColor transparent
                    ]
              , spacer
              , toggleButton_ horizontalMoreIcon (toLensVL $ showRoot % toggleShow finalLens)
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
          , widgetIf (inspectedTransaction ^. toggleShow finalLens) $
              vstack_ [childSpacing] (map (utxoRow userSymbol utxoLens) utxos)
                `styleBasic` [padding 10]
          ]

    utxoRow :: Maybe AppNode -> Lens' Transaction [TransactionUTxO] -> TransactionUTxO -> AppNode
    utxoRow userSymbol finalLens u@TransactionUTxO{..} =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf (display utxoRef) 9 white
                , filler
                , label (fromString $ printf "%D ADA" $ toAda lovelace) 
                    `styleBasic` [textSize 9]
                ]
            , hstack
                [ widgetIf (not $ null nativeAssets) $ vstack
                    [ tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                        `styleBasic` 
                          [ textSize 9
                          , textColor customBlue
                          , textFont "Remix"
                          , textMiddle
                          ]
                    , spacer_ [width 3]
                    ]
                , widgetIf (isJust datumHash) $ vstack
                    [ tooltip_ "Datum" [tooltipDelay 0] $ label datumIcon
                        `styleBasic` 
                          [ textSize 9
                          , textColor customBlue
                          , textFont "Remix"
                          , textMiddle
                          ]
                    , spacer_ [width 3]
                    ]
                , widgetIf (isJust referenceScriptHash) $ 
                    tooltip_ "Reference Script" [tooltipDelay 0] $ label scriptIcon
                      `styleBasic` 
                        [ textSize 9
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , widgetMaybe userSymbol $ \sym -> widgetIf 
                    (paymentAddress == selectedWallet ^. #paymentAddress)
                    sym
                , spacer_ [width 3]
                , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                    toggleButton_ (moreIcon showDetails)
                      (toLensVL $ #homeModel 
                                % #inspectedTransaction 
                                % toggleShow (finalLens % toggleDetails utxoRef))
                      [toggleButtonOffStyle specificUtxoMoreOffStyle]
                      `styleBasic` 
                        [ textSize 9
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
        , widgetIf showDetails $ utxoDetails reverseTickerMap u
        ]

certificateField :: Maybe Transaction -> AppNode
certificateField inspectedTransaction =
  if null certs then
    hstack
      [ label "Certificates:"
          `styleBasic`
            [ padding 0
            , radius 5
            , textMiddle
            , textSize 10
            , border 0 transparent
            , textColor customBlue
            , bgColor transparent
            ]
      , spacer
      , label "none" `styleBasic` [textColor lightGray, textSize 10]
      ]
  else
    vstack
      [ hstack
          [ label "Certificates:"
              `styleBasic`
                [ padding 0
                , radius 5
                , textMiddle
                , textSize 10
                , border 0 transparent
                , textColor customBlue
                , bgColor transparent
                ]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #homeModel % #inspectedTransaction % toggleShow #showCertificates)
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
      , widgetIf (inspectedTransaction ^. toggleShow #showCertificates) $
          vstack_ [childSpacing] (map certificateRow certs)
            `styleBasic` [padding 10]
      ]
  where
    certs :: [TransactionCertificate]
    certs = fromMaybe [] $ inspectedTransaction ^? _Just % #certificates

    certificateRow :: TransactionCertificate -> AppNode
    certificateRow TransactionCertificate{..} =
      vstack
        [ hstack
            [ label (display certificateType)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , widgetMaybe (info ^? _DelegationInfo) $ \(poolId,stakeAddress) ->
            vstack
              [ copyableLabelFor 8 "Stake Address:" (display stakeAddress)
              , spacer_ [width 2]
              , copyableLabelFor 8 "Pool ID:" (display poolId)
              ]
        , widgetMaybe (info ^? _StakeRegistrationInfo) $ \stakeAddress ->
            copyableLabelFor 8 "Stake Address:" (display stakeAddress)
        , widgetMaybe (info ^? _OtherInfo) $ \value ->
            copyableLabelFor 8 "JSON:" (showValue value)
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

withdrawalField :: Maybe Transaction -> AppNode
withdrawalField inspectedTransaction =
  if null withdrawals then
    hstack
      [ label "Withdrawals:"
          `styleBasic`
            [ padding 0
            , radius 5
            , textMiddle
            , textSize 10
            , border 0 transparent
            , textColor customBlue
            , bgColor transparent
            ]
      , spacer
      , label "none" `styleBasic` [textColor lightGray, textSize 10]
      ]
  else
    vstack
      [ hstack
          [ label "Withdrawals:"
              `styleBasic`
                [ padding 0
                , radius 5
                , textMiddle
                , textSize 10
                , border 0 transparent
                , textColor customBlue
                , bgColor transparent
                ]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #homeModel % #inspectedTransaction % toggleShow #showWithdrawals)
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
      , widgetIf (inspectedTransaction ^. toggleShow #showWithdrawals) $
          vstack_ [childSpacing] (map withdrawalRow withdrawals)
            `styleBasic` [padding 10]
      ]
  where
    withdrawals :: [TransactionWithdrawal]
    withdrawals = fromMaybe [] $ inspectedTransaction ^? _Just % #withdrawals

    withdrawalRow :: TransactionWithdrawal -> AppNode
    withdrawalRow TransactionWithdrawal{..} =
      hstack
        [ copyableLabelSelf (display stakeAddress) 10 white
        , filler
        , label (display lovelace) `styleBasic` [textSize 10]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

utxoDetails :: ReverseTickerMap -> TransactionUTxO -> AppNode
utxoDetails reverseTickerMap TransactionUTxO{..} = 
  hstack
    [ filler
    , vstack
        [ copyableLabelFor 8 "Payment Address:" (toText paymentAddress)
            `styleBasic` [padding 2]
        , copyableLabelFor 8 "Stake Address:" (maybe "none" toText stakeAddress)
            `styleBasic` [padding 2]
        , widgetMaybe referenceScriptHash $ \hash ->
            copyableLabelFor 8 "Reference Script Hash:" hash
              `styleBasic` [padding 2]
        , widgetMaybe datumHash $ \hash ->
            copyableLabelFor 8 "Datum Hash:" hash
              `styleBasic` [padding 2]
        , widgetMaybe inlineDatum $ \x ->
            copyableLabelFor 8 "Inline Datum:" (showValue x)
              `styleBasic` [padding 2]
        , widgetIf (not $ null nativeAssets) $
            vstack
              [ label "Native Assets:" `styleBasic` [textSize 8, textColor customBlue]
              , hstack
                  [ spacer_ [width 10]
                  , flip styleBasic [textSize 8, textColor lightGray, maxWidth 300] $
                      copyableTextArea $ show $ align $ vsep $ 
                        map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
                  ]
              ] `styleBasic` [padding 2]
        ] `styleBasic`
            [ bgColor black
            , padding 10
            , border 1 black
            ]
    ]

txFilterWidget :: AppModel -> AppNode
txFilterWidget model = do
  let currentScene = model ^. #homeModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #homeModel % #txFilterScene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusTL 10
                      , radiusBL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                , optionButton_ "Search" SearchScene (toLensVL $ #homeModel % #txFilterScene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusBL 10
                      , radiusTL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                ] `styleBasic` [height 100]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    , widgetIf (currentScene == SearchScene) searchWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ HomeEvent ResetTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #homeModel % #showTransactionFilter)
                    ] `styleBasic` [padding 10]
                ] `styleBasic`
                    [ bgColor customGray3
                    , radiusTL 0
                    , radiusTR 10
                    , radiusBR 10
                    , radiusBL 10
                    , border 1 black
                    ]
            , filler
            ]
        ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]
  where
    filterWidget :: AppNode
    filterWidget = do
      let rootLens = #homeModel % #txFilterModel
      vstack
        [ spacer
        , vstack
            [ hstack 
                [ spacer_ [width 20]
                , label "Start Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % lowerBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150]
                , mainButton helpIcon (Alert txStartDateMsg)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ]
            , spacer
            , hstack 
                [ spacer_ [width 20]
                , label "End Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % upperBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150]
                , mainButton helpIcon (Alert txEndDateMsg)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ]
            ]
        ]

    searchWidget :: AppNode
    searchWidget = do
      vstack_ [childSpacing]
        [ spacer
        , hstack 
            [ spacer
            , label "Find:"
            , spacer
            , textField_ 
                (toLensVL $ #homeModel % #txFilterModel % #search) 
                [placeholder "many of: address, native asset, script hash, datum hash"] 
            , mainButton helpIcon (Alert txSearchMsg)
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer
            ]
        ]

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

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Double -> Color -> WidgetNode s AppEvent
copyableLabelSelf caption fontSize color = 
  tooltip_ "Copy" [tooltipDelay 0] $ button_ caption (CopyText caption) [resizeFactor 2]
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor color
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy other data. The font size is configurable.
copyableLabelFor :: Double -> Text -> Text -> WidgetNode s AppEvent
copyableLabelFor fontSize caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button_ caption (CopyText info) [resizeFactor 0]
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          , textSize fontSize
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer_ [width 5]
    , label_ info [ellipsis,resizeFactor 2] `styleBasic` [padding 0, textColor lightGray, textSize fontSize]
    ]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `showDetails` field of the `TransactionUTxO`.
toggleDetails :: TxOutRef -> Lens' [TransactionUTxO] Bool
toggleDetails ref = lens (getToggleDetails ref) (setToggleDetails ref)
  where
    getToggleDetails :: TxOutRef -> [TransactionUTxO] -> Bool
    getToggleDetails _ [] = False
    getToggleDetails targetRef (u:us) =
      if u ^. #utxoRef == targetRef 
      then u ^. #showDetails
      else getToggleDetails targetRef us

    setToggleDetails :: TxOutRef -> [TransactionUTxO] -> Bool -> [TransactionUTxO]
    setToggleDetails _ [] _ = []
    setToggleDetails targetRef (u:us) b =
      if u ^. #utxoRef == targetRef 
      then (u & #showDetails .~ b) : us
      else u : setToggleDetails targetRef us b

-- | A lens to toggle the `show` field of the `Transaction`.
toggleShow :: Lens' Transaction Bool -> Lens' (Maybe Transaction) Bool
toggleShow finalLens = lens getToggleShow setToggleShow
  where
    getToggleShow :: Maybe Transaction -> Bool
    getToggleShow = maybe False (view finalLens)

    setToggleShow :: Maybe Transaction -> Bool -> Maybe Transaction
    setToggleShow maybeTx b = fmap (set finalLens b) maybeTx

lowerBoundText :: Lens' TxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: TxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: TxFilterModel -> Text -> TxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' TxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: TxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: TxFilterModel -> Text -> TxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _2 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Calculate the net asset flux from this address in the transaction.
txValueFromWallet :: PaymentAddress -> Transaction -> (Ada,[NativeAsset])
txValueFromWallet addr Transaction{inputs,outputs} = 
  let isFromAddress x = x ^. #paymentAddress == addr
      addressInputs = filter isFromAddress inputs
      addressOutputs = filter isFromAddress outputs
      (spentLoves,spentAssets) = 
        ( sum $ map (view #lovelace) addressInputs
        , concatMap (view #nativeAssets) addressInputs
        )
      (receivedLoves,receivedAssets) = 
        ( sum $ map (view #lovelace) addressOutputs
        , concatMap (view #nativeAssets) addressOutputs
        )
      spentMap = 
        Map.fromList $ map (\a -> (a ^. onChainName, negate $ a ^. #quantity)) spentAssets
      receivedMap = 
        Map.fromList $ map (\a -> (a ^. onChainName, a ^. #quantity)) receivedAssets
      bal = map (fromMaybe def . parseNativeAsset . \(name,q) -> show q <> " " <> name) 
          $ filter (\(_,q) -> q /= 0)
          $ Map.toList 
          $ Map.unionWith (+) spentMap receivedMap
  in (toAda $ receivedLoves - spentLoves, bal)

applySearchFilter :: PaymentWallet -> ReverseTickerMap -> [Text] -> [Transaction] -> [Transaction]
applySearchFilter _ _ [] xs = xs
applySearchFilter selectedWallet reverseTickerMap (target:ts) xs = 
  applySearchFilter selectedWallet reverseTickerMap ts $ 
    searchFilter selectedWallet reverseTickerMap target xs

matchesUTxO :: PaymentWallet -> ReverseTickerMap -> Text -> TransactionUTxO -> Bool
matchesUTxO wallet reverseTickerMap searchTarget TransactionUTxO{..} = or
    -- Only match the payment address if it is not the selected wallet's address.
    [ paymentAddress /= wallet ^. #paymentAddress && paymentAddress == PaymentAddress searchTarget
    -- Only match the stake address if it is not the selected wallet's stake address.
    , stakeAddress /= wallet ^. #stakeAddress && stakeAddress == Just (StakeAddress searchTarget)
    , maybe False (Text.isPrefixOf searchTarget) referenceScriptHash
    , maybe False (Text.isPrefixOf searchTarget) datumHash
    , Text.isPrefixOf searchTarget (display utxoRef)
    , any matchesAsset nativeAssets
    ]
  where
    matchesAsset :: NativeAsset -> Bool
    matchesAsset NativeAsset{..} = or
      [ display policyId == searchTarget
      , display tokenName == searchTarget
      , display policyId <> "." <> display tokenName == searchTarget
      , display fingerprint == searchTarget
      , Just searchTarget == fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap)
      ]

matchesCertificate :: Text -> TransactionCertificate -> Bool
matchesCertificate searchTarget TransactionCertificate{info} = or
    [ matchesDelegation
    , matchesRegistration
    ]
  where
    matchesDelegation :: Bool
    matchesDelegation = flip (maybe False) (info ^? _DelegationInfo) $ \(poolId,stakeAddress) -> or
      [ searchTarget `Text.isPrefixOf` (display poolId)
      , searchTarget `Text.isPrefixOf` (display stakeAddress)
      ]

    matchesRegistration :: Bool
    matchesRegistration = flip (maybe False) (info ^? _StakeRegistrationInfo) $ \(stakeAddress) -> 
      searchTarget `Text.isPrefixOf` (display stakeAddress)

searchFilter :: PaymentWallet -> ReverseTickerMap -> Text -> [Transaction] -> [Transaction]
searchFilter selectedWallet reverseTickerMap searchTarget xs
  | searchTarget == "" = xs
  | otherwise = flip filter xs $ \Transaction{..} -> or
      [ any (matchesUTxO selectedWallet reverseTickerMap searchTarget) inputs
      , any (matchesUTxO selectedWallet reverseTickerMap searchTarget) outputs
      , any (matchesUTxO selectedWallet reverseTickerMap searchTarget) referenceInputs
      , any (matchesCertificate searchTarget) certificates
      ]
