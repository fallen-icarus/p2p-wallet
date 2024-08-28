module P2PWallet.GUI.Widgets.Lending.Borrow.Transactions 
  ( 
    transactionsWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map
import Data.Time.Format qualified as Time

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{lendingModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      , mainWidget model `nodeVisible` (transactions /= [])
      ]
  where
    LoanWallet{transactions} = lendingModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "This wallet does not have any transactions yet"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{lendingModel=LendingModel{..},config,reverseTickerMap} =
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
                    (toLensVL $ #lendingModel % #borrowModel % #showTransactionFilter)
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
                label "No transactions found"
                 `styleBasic` [textFont "Italics"]
          ]
      , txFilterWidget model `nodeVisible` (borrowModel ^. #showTransactionFilter)
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

    (mLowerDay,mUpperDay) = borrowModel ^. #txFilterModel % #dateRange

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
    withinDateRange Transaction{blockTime} =
      blockTime >= startTime && blockTime <= endTime

    LoanWallet{loanAddress,transactions} = selectedWallet

    sample :: [Transaction]
    sample = --applySearchFilter selectedWallet reverseTickerMap txFilterModel 
      filter withinDateRange transactions

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
        , copyableLabelSelf name 8 lightGray customBlue
        , label formattedQuantity
            `styleBasic` 
              [ textSize 8, padding 3, radius 3, bgColor customGray3, textColor color]
        ] `styleBasic` [bgColor customGray4, paddingT 2, paddingB 2, paddingL 2, paddingR 0]

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let (adaFlux,assetFlux) = txValueFromWallet loanAddress tx
          adaValueColor
            | adaFlux >= 0 = customBlue
            | otherwise = customRed
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
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
            for (groupInto 2 assetFlux) $ \assetRow -> 
              hstack_ [childSpacing_ 3] $ [filler] <> map txAssetFluxWidget assetRow
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

txFilterWidget :: AppModel -> AppNode
txFilterWidget model = do
  let currentScene = model ^. #lendingModel % #borrowModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #lendingModel % #borrowModel % #txFilterScene) 
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
                ] `styleBasic` [height 50]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ LendingEvent $ BorrowEvent ResetBorrowTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #lendingModel % #borrowModel % #showTransactionFilter)
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
      let rootLens = #lendingModel % #borrowModel % #txFilterModel
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , vstack
            [ hstack 
                [ spacer_ [width 20]
                , label "Start Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % lowerBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
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
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
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

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
lowerBoundText :: Lens' BorrowTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: BorrowTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: BorrowTxFilterModel -> Text -> BorrowTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' BorrowTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: BorrowTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: BorrowTxFilterModel -> Text -> BorrowTxFilterModel
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
        , concatMap (map (over #quantity negate) . view #nativeAssets) addressInputs
        )
      (receivedLoves,receivedAssets) = 
        ( sum $ map (view #lovelace) addressOutputs
        , concatMap (view #nativeAssets) addressOutputs
        )
      noBeacons = \NativeAsset{policyId} -> and
        [ policyId /= Loans.negotiationBeaconCurrencySymbol
        , policyId /= Loans.activeBeaconCurrencySymbol
        ]
      assetChange = filter noBeacons $ sumNativeAssets $ spentAssets <> receivedAssets
  in (toAda $ receivedLoves - spentLoves, assetChange)

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Double -> Color -> Color -> WidgetNode s AppEvent
copyableLabelSelf caption fontSize mainColor hoverColor = 
  tooltip_ "Copy" [tooltipDelay 0] $ button_ caption (CopyText caption) [resizeFactor 2]
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor hoverColor, cursorIcon CursorHand]
