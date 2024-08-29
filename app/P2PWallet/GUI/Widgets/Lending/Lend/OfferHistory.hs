module P2PWallet.GUI.Widgets.Lending.Lend.OfferHistory 
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
      [ noTransactions `nodeVisible` null offerTransactions
      , mainWidget model `nodeVisible` (offerTransactions /= [])
      ]
  where
    LoanWallet{offerTransactions} = lendingModel ^. #selectedWallet 

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
                    (toLensVL $ #lendingModel % #lendModel % #showTransactionFilter)
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
      , txFilterWidget model `nodeVisible` (model ^. #lendingModel % #lendModel % #showTransactionFilter)
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

    (mLowerDay,mUpperDay) = lendModel ^. #txFilterModel % #dateRange

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

    LoanWallet{stakeCredential,offerTransactions} = selectedWallet
    lenderId = Loans.genLenderId stakeCredential

    sample :: [Transaction]
    sample = applySearchFilter reverseTickerMap lenderId (lendModel ^. #txFilterModel)
           $ filter withinDateRange offerTransactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let (closedCount,createdCount,acceptedCount) = offerActionCount lenderId tx
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
            -- , spacer_ [width 2]
            -- , widgetIf (executedCount > 0) $ tooltip_ "Inspect Swap Executions" [tooltipDelay 0] $
            --     button inspectIcon (DexEvent $ InspectDexTransaction tx)
            --       `styleBasic` 
            --         [ textSize 10
            --         , textColor customBlue
            --         , textFont "Remix"
            --         , textMiddle
            --         , padding 0
            --         , paddingL 2
            --         , paddingR 2
            --         , bgColor transparent
            --         , border 0 transparent
            --         ]
            --       `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label calendarIcon
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
                  , textColor white
                  ]
            , spacer_ [width 5]
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
                  , textColor white
                  ]
            ]
        , hstack
            [ label (show closedCount <> " Offer(s) Closed")
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label "/"
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label (show createdCount <> " Offer(s) Created")
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            , label (show acceptedCount <> " Offer(s) Accepted")
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

txFilterWidget :: AppModel -> AppNode
txFilterWidget model = do
  let currentScene = model ^. #lendingModel % #lendModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #lendingModel % #lendModel % #txFilterScene) 
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
                , optionButton_ "Search" SearchScene (toLensVL $ #lendingModel % #lendModel % #txFilterScene) 
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
                    , button "Reset" $ LendingEvent $ LendEvent ResetLendTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #lendingModel % #lendModel % #showTransactionFilter)
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
      let rootLens = #lendingModel % #lendModel % #txFilterModel
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

    searchWidget :: AppNode
    searchWidget = do
      cushionWidgetH $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Search Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert filterOfferTxByLoanAssetMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Loan Asset:"
                `styleBasic` [textSize 12]
            , spacer_ [width 5]
            , textField_ (toLensVL $ #lendingModel % #lendModel % #txFilterModel % #loanAsset)
                  [placeholder "ADA"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert filterOfferTxByCollateralMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Collateral Assets (separated with newlines)"
                `styleBasic` [textSize 12]
            ]
        , spacer
        , textArea (toLensVL $ #lendingModel % #lendModel % #txFilterModel % #collateral)
            `styleBasic` [height 180, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | (# Offers Closed, # Offers Created, # Offers Accepted).
offerActionCount :: Loans.LenderId -> Transaction -> (Int,Int,Int)
offerActionCount lenderId Transaction{plutusContracts,outputs} =
    (closedCount, createdCount, acceptedCount)
  where
    checkSpend :: (Int,Int) -> TransactionPlutusContract -> (Int,Int)
    checkSpend acc@(closed,accepted) TransactionPlutusContract{..} =
     case decodeData @Loans.LoanRedeemer redeemer of
        Nothing -> acc
        Just r -> case r of
          Loans.CloseOrUpdateOffer -> 
            case datum >>= decodeData @Loans.OfferDatum of
              Nothing -> acc
              Just offerDatum -> 
                if lenderId == offerDatum ^. #lenderId then
                  (closed + 1, accepted)
                else 
                  acc
          Loans.AcceptOffer ->
            case datum >>= decodeData @Loans.OfferDatum of
              Nothing -> acc
              Just offerDatum -> 
                if lenderId == offerDatum ^. #lenderId then
                  (closed, accepted + 1)
                else 
                  acc
          _ -> acc

    (closedCount, acceptedCount) = foldl' checkSpend (0,0) plutusContracts

    checkCreation :: Int -> TransactionUTxO -> Int
    checkCreation acc TransactionUTxO{nativeAssets} =
      let hasLenderId = flip any nativeAssets $ \NativeAsset{policyId,tokenName} -> and
            [ policyId == Loans.negotiationBeaconCurrencySymbol
            , tokenName == Loans.unLenderId lenderId
            ]
       in if hasLenderId then acc + 1 else acc

    createdCount = foldl' checkCreation 0 outputs

matchesUTxO 
  :: ReverseTickerMap 
  -> Loans.LenderId
  -> LendTxFilterModel
  -> TransactionUTxO 
  -> Bool
matchesUTxO reverseTickerMap lenderId LendTxFilterModel{..} TransactionUTxO{..} = 
    and
      [ matchesAsset [actualLoanAsset] loanAsset
      , matchesAsset actualCollateral collateral
      , offerDatum ^. #lenderId == lenderId 
      ]
  where
    matchesAsset :: [NativeAsset] -> Text -> Bool
    matchesAsset xs searchTarget
      | searchTarget == "" = True
      | otherwise = flip any xs $ \NativeAsset{..} -> or
          [ display policyId <> "." <> display tokenName == searchTarget
          , Just searchTarget ==
              fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap) 
          , policyId == "" && searchTarget == "ADA"
          ]

    offerDatum = fromMaybe def $ inlineDatum >>= decodeData @Loans.OfferDatum

    actualLoanAsset = toNativeAsset $ offerDatum ^. #loanAsset
    actualCollateral =
      map (toNativeAsset . fst) $ Loans.unCollateralization $ offerDatum ^. #collateralization 

applySearchFilter 
  :: ReverseTickerMap 
  -> Loans.LenderId
  -> LendTxFilterModel 
  -> [Transaction] 
  -> [Transaction]
applySearchFilter reverseTickerMap lenderId filterModel xs
  | filterModel ^. #loanAsset == "" && filterModel ^. #collateral == "" = xs
  | otherwise = flip filter xs $ \Transaction{..} -> or
      [ any (matchesUTxO reverseTickerMap lenderId filterModel) inputs
      , any (matchesUTxO reverseTickerMap lenderId filterModel) outputs
      ]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
lowerBoundText :: Lens' LendTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: LendTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: LendTxFilterModel -> Text -> LendTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' LendTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: LendTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: LendTxFilterModel -> Text -> LendTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _2 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

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
