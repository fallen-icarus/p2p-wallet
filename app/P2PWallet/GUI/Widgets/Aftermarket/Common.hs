{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.GUI.Widgets.Aftermarket.Common
  ( inspectBatchWidget
  , InspectBatchConfig(..)
  , nftsRow
  , inspectSellerWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.SellerInformation
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

data InspectBatchConfig = InspectBatchConfig
  { batchUTxO :: AftermarketUTxO
  , closeEvent :: AppEvent
  , inspectLoanHistoryEvent :: Loans.LoanId -> AppEvent
  , lookupBorrowerEvent :: (Loans.BorrowerId,PaymentAddress) -> AppEvent
  , mAddToHomeEvent :: Maybe (NativeAsset -> AppEvent)
  }

makeFieldLabelsNoPrefix ''InspectBatchConfig

inspectBatchWidget :: AppModel -> InspectBatchConfig -> AppNode
inspectBatchWidget model iCfg@InspectBatchConfig{..} = do
    vstack
      [ centerWidget $ vstack
          [ centerWidgetH $ hstack
              [ label header
                  `styleBasic` [textFont "Italics", textColor customBlue]
              , spacer_ [width 3]
              , tooltip_ "Resync Info" [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ AftermarketEvent $ LookupKeyInfo (True,batchUTxO)] $
                    label refreshIcon
                      `styleBasic` 
                        [ border 0 transparent
                        , radius 20
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , padding 3
                        , textSize 12
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , spacer
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] batchNode
          , spacer
          , box_ [alignRight] $ 
              button "Close" closeEvent
                `styleBasic` [textSize 10]
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
    (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts batchUTxO
    header = "Aftermarket NFT Batch"
    batchNode
      | policyId == Loans.activeBeaconCurrencySymbol = map (inspectLoanWidget model iCfg) names
      | policyId == Options.activeBeaconCurrencySymbol = map (inpsectOptionsWidget model iCfg) names
      | otherwise = map (inpsectUnknownNftWidget policyId iCfg) names

inspectLoanWidget :: AppModel -> InspectBatchConfig -> TokenName -> AppNode
inspectLoanWidget AppModel{..} InspectBatchConfig{..} tokenName =
    hstack
      [ activeStatus loanUTxO
      , widgetMaybe mAddToHomeEvent $ \addEvent ->
          hstack
            [ spacer_ [width 3]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Add to Home Batch" [tooltipDelay 0] $
                mainButton addIcon (addEvent loanIdNft)
                  `styleBasic` 
                    [ textSize 10
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    ]
                  `styleHover` [cursorIcon CursorHand]
            ]
      ]
  where
    Config{currentTime} = config

    loanIdNft = mkNativeAsset Loans.activeBeaconCurrencySymbol tokenName & #quantity .~ 1

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
    activeStatus u@LoanUTxO{loanAddress,utxoRef,lovelace=utxoLovelace,nativeAssets=utxoNativeAssets} = do
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
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
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
                tooltip_ ("Loan ID: " <> display loanId <> " (history)") [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ inspectLoanHistoryEvent loanId] $
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
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ "Lookup Borrower Information" [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ lookupBorrowerEvent (borrowerId,loanAddress)] $
                    label idCardIcon
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
            , widgetIf (loanExpiration <= toPlutusTime currentTime) $
                hstack
                  [ filler
                  , label "Expired"
                      `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                  , filler
                  ]
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
            [ widgetIf collateralIsSwappable $ box_ [alignTop] $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        , paddingT 1
                        ]
                , spacer_ [width 2]
                ]
            , box_ [alignTop] $ label "Locked Collateral:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
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

inpsectOptionsWidget :: AppModel -> InspectBatchConfig -> TokenName -> AppNode
inpsectOptionsWidget AppModel{..} InspectBatchConfig{..} tokenName =
    hstack
      [ activeStatus optionsUTxO
      , widgetMaybe mAddToHomeEvent $ \addEvent ->
          hstack
            [ spacer_ [width 3]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Add to Home Batch" [tooltipDelay 0] $
                mainButton addIcon (addEvent contractIdNft)
                  `styleBasic` 
                    [ textSize 10
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    ]
                  `styleHover` [cursorIcon CursorHand]
            ]
      ]
  where
    optionsUTxO = maybe def (fromMaybe def)
                $ Map.lookup (Options.ContractId tokenName) 
                $ optionsModel ^. #cachedKeyContracts

    contractIdNft = mkNativeAsset Options.activeBeaconCurrencySymbol tokenName & #quantity .~ 1

    Config{network,currentTime} = config

    activeStatus :: OptionsUTxO -> AppNode
    activeStatus u@OptionsUTxO{..} = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
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
            , widgetIf (expiration <= toPlutusTime currentTime) $
                hstack
                  [ filler
                  , label "Expired"
                      `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                  , filler
                  ]
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

inpsectUnknownNftWidget :: CurrencySymbol -> InspectBatchConfig -> TokenName -> AppNode
inpsectUnknownNftWidget policyId InspectBatchConfig{..} tokenName =
    hstack
      [ assetRow
      , widgetMaybe mAddToHomeEvent $ \addEvent ->
          hstack
            [ spacer_ [width 3]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Add to Home Batch" [tooltipDelay 0] $
                mainButton addIcon (addEvent nft)
                  `styleBasic` 
                    [ textSize 10
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    ]
                  `styleHover` [cursorIcon CursorHand]
            ]
      ]
  where
    nft@NativeAsset{fingerprint} = mkNativeAsset policyId tokenName & #quantity .~ 1

    assetRow :: AppNode
    assetRow = do
      vstack
        [ hstack 
            [ copyableLabelMain (display fingerprint)
                `styleBasic` [textSize 10]
            , filler
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

nftsRow :: Bool -> (NativeAsset -> AppEvent) -> AppModel -> NativeAsset -> AppNode
nftsRow canRemoveNft removeEvt AppModel{..} nft@NativeAsset{..} = do
    hstack
      [ if policyId == Loans.activeBeaconCurrencySymbol then 
          activeLoanStatus loanUTxO
        else if policyId == Options.activeBeaconCurrencySymbol then 
          activeOptionsStatus optionsUTxO
        else 
          assetRow
      , spacer_ [width 3]
      , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Key NFT" [tooltipDelay 0] $
          button closeCircleIcon (removeEvt nft)
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
            `nodeVisible` canRemoveNft
      ]
  where
    Config{network,currentTime} = config

    activeLoanStatus :: LoanUTxO -> AppNode
    activeLoanStatus u@LoanUTxO{utxoRef,lovelace=utxoLovelace,nativeAssets=utxoNativeAssets} = do
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
          addressTip = case mTargetWallet of
            Nothing -> "Payments to: " <> display payToAddress
            Just w -> unwords
              [ "Payments to"
              , w ^. #alias <> ":"
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
            , widgetIf (loanExpiration <= toPlutusTime currentTime) $
                hstack
                  [ filler
                  , label "Expired"
                      `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                  , filler
                  ]
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
            [ widgetIf collateralIsSwappable $ box_ [alignTop] $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        , paddingT 1
                        ]
                , spacer_ [width 2]
                ]
            , box_ [alignTop] $ label "Locked Collateral:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
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

    optionsUTxO = maybe def (fromMaybe def)
                $ Map.lookup (Options.ContractId tokenName) 
                $ optionsModel ^. #cachedKeyContracts

    activeOptionsStatus :: OptionsUTxO -> AppNode
    activeOptionsStatus u@OptionsUTxO{..} = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = case mTargetWallet of
            Nothing -> "Payments to: " <> display payToAddress
            Just w -> unwords
              [ "Payments to"
              , w ^. #alias <> ":"
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

moreOffStyle :: Style
moreOffStyle = 
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

inspectSellerWidget 
  :: PaymentAddress
  -> AppEvent 
  -> (AftermarketUTxO -> AppEvent) 
  -> (AftermarketUTxO -> AppEvent) 
  -> AppModel 
  -> AppNode
inspectSellerWidget sellerAddr closeEvt inspectSaleEvt inspectBidEvt model = do
    vstack
      [ vstack
          [ centerWidgetH $ hstack
              [ label "Seller Status"
                `styleBasic` [textFont "Italics", textColor customBlue]
              , spacer_ [width 5]
              , tooltip_ "Resync Information" [tooltipDelay 0] $
                  let resyncEvt = AftermarketEvent $ LookupSellerInfo $ StartProcess $ Just sellerAddr in
                  box_ [alignMiddle, onClick resyncEvt] $
                    label refreshIcon
                      `styleBasic` 
                        [ border 0 transparent
                        , radius 20
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , padding 3
                        , textSize 12
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , spacer
          , vscroll_ [wheelRate 50] $ vstack
              [ currentSalesWidget sellerAddr inspectSaleEvt model
              , spacer
              , currentBidsWidget sellerAddr inspectBidEvt model
              ]
          , filler
          , hstack
              [ filler
              , button "Close" closeEvt
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

currentSalesWidget :: PaymentAddress -> (AftermarketUTxO -> AppEvent) -> AppModel -> AppNode
currentSalesWidget sellerAddr inspectEvt AppModel{..} = do
    vstack
      [ hstack
          [ label "Current Sales:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #aftermarketModel % #cachedSellerInfo % at sellerAddr % toggleShow #showCurrentSales)
              [toggleButtonOffStyle moreOffStyle]
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
              `nodeVisible` (currentSales /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null currentSales
          ]
      , widgetIf showCurrentSales $
          vstack_ [childSpacing] (map saleRow currentSales)
            `styleBasic` [padding 10]
      ]
  where
    Config{network} = config

    SellerInformation{..} = fromMaybe def 
                          $ Map.lookup sellerAddr (aftermarketModel ^. #cachedSellerInfo)

    saleRow :: AftermarketUTxO -> AppNode
    saleRow u@AftermarketUTxO{marketDatum} = case marketDatum of
      (Just (SpotDatum spotDatum)) -> spotRow u spotDatum
      (Just (AuctionDatum auctionDatum)) -> auctionRow u auctionDatum
      _ -> spacer

    priceWidget :: NativeAsset -> AppNode
    priceWidget priceAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap priceAsset)
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

    spotRow :: AftermarketUTxO -> Aftermarket.SpotDatum -> AppNode
    spotRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.SpotDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
            , display payToAddress
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ salePrice ^. #unPrices
      vstack
        [ hstack
            [ label "Spot"
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
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
            , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ inspectEvt u] $
                  label inspectIcon
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
            , label (show numNfts <> " " <> keyTypeLabel)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ box_ [alignTop] $ label "Sale Price:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]
    
    auctionRow :: AftermarketUTxO -> Aftermarket.AuctionDatum -> AppNode
    auctionRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.AuctionDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ startingPrice ^. #unPrices
      vstack
        [ hstack
            [ label "Auction"
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
                    ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ inspectEvt u] $
                  label inspectIcon
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
            , label (show numNfts <> " " <> keyTypeLabel)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ box_ [alignTop] $ label "Starting Price:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

currentBidsWidget :: PaymentAddress -> (AftermarketUTxO -> AppEvent) -> AppModel -> AppNode
currentBidsWidget sellerAddr inspectEvt AppModel{..} = do
    vstack
      [ hstack
          [ label "Current Bids:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #aftermarketModel % #cachedSellerInfo % at sellerAddr % toggleShow #showCurrentBids)
              [toggleButtonOffStyle moreOffStyle]
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
              `nodeVisible` (currentBids /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null currentBids
          ]
      , widgetIf showCurrentBids $
          vstack_ [childSpacing] (map bidRow currentBids)
            `styleBasic` [padding 10]
      ]
  where
    Config{network,currentTime} = config

    SellerInformation{..} = fromMaybe def 
                          $ Map.lookup sellerAddr (aftermarketModel ^. #cachedSellerInfo)

    bidRow :: AftermarketUTxO -> AppNode
    bidRow u@AftermarketUTxO{marketDatum} = case marketDatum of
      (Just (SpotBidDatum spotBidDatum)) -> spotBidRow u spotBidDatum
      (Just (ClaimBidDatum claimBidDatum)) -> claimBidRow u claimBidDatum
      (Just (AcceptedBidDatum acceptedBidDatum)) -> acceptedBidRow u acceptedBidDatum
      _ -> spacer `nodeVisible` False

    priceWidget :: NativeAsset -> AppNode
    priceWidget priceAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap priceAsset)
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

    spotBidRow :: AftermarketUTxO -> Aftermarket.SpotBidDatum -> AppNode
    spotBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.SpotBidDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
            , display payToAddress
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
      vstack
        [ hstack
            [ label "Spot Bid"
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
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
            , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ inspectEvt u] $
                  label inspectIcon
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
            , label (show numNfts <> " " <> keyTypeLabel)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ box_ [alignTop] $ label "Bid:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

    claimBidRow :: AftermarketUTxO -> Aftermarket.ClaimBidDatum -> AppNode
    claimBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.ClaimBidDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
          prettyExpirationTime = unlines
            [ unwords
                [ "Bid Expires:"
                , flip (maybe "Never") bidExpiration $ \expr -> unwords
                    [ showLocalDate (config ^. #timeZone) $ fromPlutusTime expr
                    , showLocalTime (config ^. #timeZone) $ fromPlutusTime expr
                    ]
                ]
            , unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
      vstack
        [ hstack
            [ label "Claim Bid"
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
                    ]
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
            , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ inspectEvt u] $
                  label inspectIcon
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
            , widgetIf (maybe False (<= toPlutusTime currentTime) bidExpiration) $
                hstack
                  [ filler
                  , label "Expired"
                      `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                  , filler
                  ]
            , filler
            , label (show numNfts <> " " <> keyTypeLabel)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ box_ [alignTop] $ label "Bid:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
            , filler
            , label "Deposit:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , label (display $ Lovelace bidDeposit)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

    acceptedBidRow :: AftermarketUTxO -> Aftermarket.AcceptedBidDatum -> AppNode
    acceptedBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.AcceptedBidDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
            , display payToAddress
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
          prettyExpirationTime = unlines
            [ unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
      vstack
        [ hstack
            [ label "Accepted Claim Bid"
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
                    ]
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
            , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ inspectEvt u] $
                  label inspectIcon
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
            , widgetIf (claimExpiration <= toPlutusTime currentTime) $
                hstack
                  [ filler
                  , label "Expired"
                      `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                  , filler
                  ]
            , filler
            , label (show numNfts <> " " <> keyTypeLabel)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ box_ [alignTop] $ label "Bid:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
            , filler
            , label "Deposit:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , label (display $ Lovelace bidDeposit)
                `styleBasic` [textSize 8, textColor lightGray]
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

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `show` field of the `Transaction`.
toggleShow :: Lens' SellerInformation Bool -> Lens' (Maybe SellerInformation) Bool
toggleShow finalLens = lens getToggleShow setToggleShow
  where
    getToggleShow :: Maybe SellerInformation -> Bool
    getToggleShow = maybe False (view finalLens)

    setToggleShow :: Maybe SellerInformation -> Bool -> Maybe SellerInformation
    setToggleShow maybeInfo b = fmap (set finalLens b) maybeInfo
