module P2PWallet.GUI.Widgets.Home.NativeAssets.NftBatch
  ( 
    createSaleWidget
  , viewBatchWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

createSaleWidget :: AppModel -> AppNode
createSaleWidget model@AppModel{..} = do
  let isAuction = fromMaybe False $ homeModel ^? #newSaleCreation % _Just % #isAuction
      maybeLens' = maybeLens def (#homeModel % #newSaleCreation)
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  vstack
    [ centerWidget $ vstack
        [ hgrid
            [ optionButton_ "Spot" False (toLensVL $ maybeLens' % #isAuction) 
                [optionButtonOffStyle offStyle]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , radiusTL 10
                  , radiusTR 0
                  , radiusBR 0
                  , radiusBL 0
                  , border 0 transparent
                  ]
            , optionButton_ "Auction" True (toLensVL $ maybeLens' % #isAuction)
                [optionButtonOffStyle offStyle]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , radiusTL 0
                  , radiusTR 10
                  , radiusBR 0
                  , radiusBL 0
                  , border 0 transparent
                  ]
            ]
        , zstack 
            [ widgetIf (not isAuction) $ centerWidgetH $ spotCreationWidget model
            , widgetIf isAuction $ centerWidgetH $ auctionCreationWidget model
            ] `styleBasic` 
                [ bgColor customGray3
                , paddingT 0
                , paddingL 20
                , paddingR 20
                , paddingB 20
                , radiusTL 0
                , radiusTR 0
                , radiusBL 10
                , radiusBR 10
                ]
        ] 
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

spotCreationWidget :: AppModel -> AppNode
spotCreationWidget AppModel{..} = do
  let maybeLens' = maybeLens def (#homeModel % #newSaleCreation)
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
      helpButton msg = box_ [alignMiddle, onClick $ Alert msg] $
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
  vstack
    [ centerWidgetH $ hstack
        [ label "What is a spot sale?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsSpotSaleMsg
        ]
    , spacer
    , hstack
        [ label "Seller Credential:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % #marketWallet) 
              (knownWallets ^. #marketWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 3]
        , helpButton sellerCredentialMsg
        ]
    , spacer
    , hstack
        [ label "Payment Address:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % #paymentWallet) 
              (knownWallets ^. #paymentWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 3]
        , helpButton salePaymentAddressMsg
        ]
    , spacer
    , hstack
        [ label "Sale Price (assets separated with newlines)"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton salePriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % #salePrice)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (HomeEvent $ NftBatchEvent $ ConfirmNftBatch CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create Spot" (HomeEvent $ NftBatchEvent $ ConfirmNftBatch ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

auctionCreationWidget :: AppModel -> AppNode
auctionCreationWidget AppModel{..} = do
  let maybeLens' = maybeLens def (#homeModel % #newSaleCreation)
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
      helpButton msg = box_ [alignMiddle, onClick $ Alert msg] $
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
  vstack
    [ centerWidgetH $ hstack
        [ label "What is an auction sale?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsAuctionSaleMsg
        ]
    , spacer
    , hstack
        [ label "Seller Credential:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % #marketWallet) 
              (knownWallets ^. #marketWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 3]
        , helpButton sellerCredentialMsg
        ]
    , spacer
    , hstack
        [ label "Starting Price (assets separated with newlines)"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton startingPriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % #salePrice)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (HomeEvent $ NftBatchEvent $ ConfirmNftBatch CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create Auction" (HomeEvent $ NftBatchEvent $ ConfirmNftBatch ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

viewBatchWidget :: AppModel -> AppNode
viewBatchWidget model@AppModel{homeModel} = do
    vstack
      [ centerWidget $ vstack
          [ centerWidgetH $ label header
              `styleBasic` [textFont "Italics", textColor customBlue]
          , spacer
          , widgetIf (queue /= []) $
              flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
                vstack_ [childSpacing] batchNode
          , widgetIf (null queue) $ 
              centerWidget $ 
                label "Empty"
                  `styleBasic` [textFont "Italics", textSize 14, textColor lightGray]
          , spacer
          , hstack
              [ button "Close" (HomeEvent $ NftBatchEvent CloseNftBatchView)
                  `styleBasic` [textSize 10]
              , filler
              , widgetIf (queue /= []) $
                  tooltip_ "Create Sale/Auction" [tooltipDelay 0] $
                    mainButton "Checkout" 
                      (HomeEvent $ NftBatchEvent $ ConfirmNftBatch $ StartAdding $ Just queue)
                      `styleBasic` [textSize 10]
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
    queue = homeModel ^. #queuedNFTs
    keyPolicy = maybe "" (view #policyId) $ maybeHead queue
    header = "Aftermarket NFT Batch"
    batchNode
      | keyPolicy == Loans.activeBeaconCurrencySymbol = map (viewLoanWidget model) queue
      | keyPolicy == Options.activeBeaconCurrencySymbol = map (viewOptionsWidget model) queue
      | otherwise = map (viewUnknownNftWidget model) queue

viewLoanWidget :: AppModel -> NativeAsset -> AppNode
viewLoanWidget AppModel{..} nft@NativeAsset{tokenName} = do
    hstack
      [ activeStatus loanUTxO
      , spacer_ [width 3]
      , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Key NFT" [tooltipDelay 0] $
          button closeCircleIcon (HomeEvent $ NftBatchEvent $ RemoveNftFromBatch nft)
            `styleBasic` 
              [ textSize 10
              , textColor customRed
              , textFont "Remix"
              , textMiddle
              , padding 3
              , radius 3
              , bgColor transparent
              , border 0 transparent
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
      ]
  where
    loanUTxO = fromMaybe def
             $ snd =<< Map.lookup (Loans.LoanId tokenName) (lendingModel ^. #cachedLoanHistories)

    lockedCollateralWidget :: NativeAsset -> AppNode
    lockedCollateralWidget collateralAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap collateralAsset)
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

    activeStatus :: LoanUTxO -> AppNode
    activeStatus u@LoanUTxO{utxoRef,lovelace=utxoLovelace,nativeAssets=utxoNativeAssets} = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum u
          loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
          expiration = fromPlutusTime loanExpiration
          claimDeadline = fromPlutusTime claimExpiration
          mNextEpochBoundary = (+lastEpochBoundary) <$> epochDuration
          nextPaymentDueDate = case mNextEpochBoundary of
            Nothing -> expiration
            Just nextCompounding -> min (fromPlutusTime nextCompounding) expiration
          amountDue = minPayment - totalEpochPayments
          nextPaymentSize
            | minPayment == 0 = loanBalance
            | amountDue < 0 = loanBalance & #quantity .~ 0
            | otherwise = loanBalance & #quantity .~ amountDue
          prettyExpirationTime = unlines
            [ unwords
                [ "Expires:"
                , showLocalDate (config ^. #timeZone) expiration
                , showLocalTime (config ^. #timeZone) expiration
                ]
            , unwords
                [ "Claim Expiration:"
                , showLocalDate (config ^. #timeZone) claimDeadline
                , showLocalTime (config ^. #timeZone) claimDeadline
                ]
            ]
          prettyNextPaymentDueDate = unwords
            [ "Next Deadline:"
            , ""
            , showLocalDate (config ^. #timeZone) nextPaymentDueDate
            , showLocalTime (config ^. #timeZone) nextPaymentDueDate
            ]
          prettyInterest 
            | loanInterest == 0 = "Interest-Free"
            | otherwise = unwords
                [ if compoundingInterest then "Compounding" else "Non-Compounding"
                , "Interest:"
                , displayPercentage (toRational loanInterest) <> "%"
                ]
          prettyNextPayment = unwords
            [ "Amount Required by Deadline:"
            , ""
            , showAssetBalance True reverseTickerMap nextPaymentSize
            ]
          prettyEpochDuration = flip (maybe "No Loan Epochs") epochDuration $ \freq ->
            unwords
              [ "Loan Epoch:"
              , show (calcDaysInPosixPeriod $ fromPlutusTime freq)
              , "Day(s)"
              ]
          prettyPenalty = case penalty of
            Loans.NoPenalty -> "No Penalty"
            Loans.FixedFee fee -> unwords
              [ "Fee Penalty:"
              , showAssetBalance True reverseTickerMap $ loanBalance & #quantity .~ fee
              ]
            Loans.PercentFee percent -> unwords
              [ "Percent Penalty:"
              , displayPercentage (toRational percent) <> "%"
              ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          allAssets = 
            (lovelaceAsNativeAsset & #quantity .~ unLovelace utxoLovelace) : utxoNativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
          payToAddress = either (const "error") fst $ plutusToBech32 (config ^. #network) lenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Balance: " <> showAssetBalance True reverseTickerMap loanBalance)
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , let prettyRef = display utxoRef in
              flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
                  label targetUTxOIcon
                    `styleBasic` 
                      [ bgColor black
                      , textMiddle
                      , textFont "Remix"
                      , textSize 8
                      , textColor customBlue
                      , paddingT 1
                      , paddingB 1
                      , paddingL 3
                      , paddingR 3
                      , radius 5
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ prettyExpirationTime [tooltipDelay 0] $
                  label expirationIcon
                    `styleBasic` 
                      [ textMiddle
                      , textFont "Remix"
                      , textSize 10
                      , textColor customRed
                          ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ tooltip_ addressTip [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display payToAddress] $
                  label targetAddressIcon
                    `styleBasic` 
                      [ bgColor black
                      , textMiddle
                      , textFont "Remix"
                      , textSize 8
                      , textColor customBlue
                      , paddingT 1
                      , paddingB 1
                      , paddingL 3
                      , paddingR 3
                      , radius 5
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Loan ID: " <> display loanId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ CopyText $ display loanId] $
                    label keyNftIcon
                      `styleBasic` 
                        [ bgColor black
                        , textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        , paddingT 1
                        , paddingB 1
                        , paddingL 3
                        , paddingR 3
                        , radius 5
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label prettyNextPaymentDueDate
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 3]
        , hstack
            [ label prettyInterest
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyEpochDuration
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust epochDuration) $ vstack
            [ spacer_ [width 3]
            , hstack
                [ label prettyNextPayment
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label prettyPenalty
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ]
        , spacer_ [width 2]
        , hstack
            [ widgetIf collateralIsSwappable $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        ]
                , spacer_ [width 2]
                ]
            , label "Locked Collateral:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 lockedCollateral) $ 
                \col -> hstack_ [childSpacing_ 3] $ map lockedCollateralWidget col
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

viewOptionsWidget :: AppModel -> NativeAsset -> AppNode
viewOptionsWidget AppModel{..} nft@NativeAsset{tokenName} = do
    hstack
      [ activeStatus optionsUTxO
      , spacer_ [width 3]
      , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Key NFT" [tooltipDelay 0] $
          button closeCircleIcon (HomeEvent $ NftBatchEvent $ RemoveNftFromBatch nft)
            `styleBasic` 
              [ textSize 10
              , textColor customRed
              , textFont "Remix"
              , textMiddle
              , padding 3
              , radius 3
              , bgColor transparent
              , border 0 transparent
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
      ]
  where
    optionsUTxO = maybe def (fromMaybe def)
                $ Map.lookup (Options.ContractId tokenName) 
                $ optionsModel ^. #cachedKeyContracts

    Config{network} = config

    activeStatus :: OptionsUTxO -> AppNode
    activeStatus u@OptionsUTxO{..} = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
          formattedPrice = showPriceFormatted reverseTickerMap askNativeAsset offerAmount 
                         $ toRational strikePrice
          prettyPrice = mconcat
            [ "Strike Price: "
            , formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askNativeAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAmount
            ]
          prettyExpirationTime = unwords
            [ "Expiration:"
            , showLocalDate (config ^. #timeZone) $ fromPlutusTime expiration
            , showLocalTime (config ^. #timeZone) $ fromPlutusTime expiration
            ]
      vstack
        [ hstack
            [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , let prettyRef = display utxoRef in
              flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
                  label targetUTxOIcon
                    `styleBasic` 
                      [ bgColor black
                      , textMiddle
                      , textFont "Remix"
                      , textSize 8
                      , textColor customBlue
                      , paddingT 1
                      , paddingB 1
                      , paddingL 3
                      , paddingR 3
                      , radius 5
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ tooltip_ addressTip [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display payToAddress] $
                  label targetAddressIcon
                    `styleBasic` 
                      [ bgColor black
                      , textMiddle
                      , textFont "Remix"
                      , textSize 8
                      , textColor customBlue
                      , paddingT 1
                      , paddingB 1
                      , paddingL 3
                      , paddingR 3
                      , radius 5
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Options Contract ID: " <> display contractId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ CopyText $ display contractId] $
                    label keyNftIcon
                      `styleBasic` 
                        [ bgColor black
                        , textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        , paddingT 1
                        , paddingB 1
                        , paddingL 3
                        , paddingR 3
                        , radius 5
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label ("Ask Asset: " <> showAssetNameOnly reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label prettyPrice
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyExpirationTime
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

viewUnknownNftWidget :: AppModel -> NativeAsset -> AppNode
viewUnknownNftWidget AppModel{..} nft@NativeAsset{..} = do
    hstack
      [ assetRow
      , spacer_ [width 3]
      , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Key NFT" [tooltipDelay 0] $
          button closeCircleIcon (HomeEvent $ NftBatchEvent $ RemoveNftFromBatch nft)
            `styleBasic` 
              [ textSize 10
              , textColor customRed
              , textFont "Remix"
              , textMiddle
              , padding 3
              , radius 3
              , bgColor transparent
              , border 0 transparent
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
      ]
  where
    assetRow :: AppNode
    assetRow = do
      vstack
        [ hstack 
            [ copyableLabelMain (display fingerprint)
                `styleBasic` [textSize 10]
            , filler
              -- Show the asset name with the ticker if set. Do not use the fingerprint otherwise.
            , label (showAssetBalance False reverseTickerMap nft)
                `styleBasic` [textSize 10]
            ]
        , spacer_ [width 2]
        , hstack 
            [ label idCardIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , copyableLabelSub $ display policyId <> "." <> display tokenName
            , filler
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

-------------------------------------------------
-- Helper Widget
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelMain :: Text -> WidgetNode s AppEvent
copyableLabelMain caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 10
      , border 0 transparent
      , textColor customBlue
      , bgColor transparent
      ]
    `styleHover` [textColor white, cursorIcon CursorHand]

-- | A label button that will copy itself.
copyableLabelSub :: Text -> WidgetNode s AppEvent
copyableLabelSub caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 8
      , border 0 transparent
      , textColor lightGray
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
