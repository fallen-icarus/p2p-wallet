module P2PWallet.GUI.Widgets.Home.NativeAssets.LoanKeys
  ( 
    inspectLoanWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

inspectLoanWidget :: AppModel -> AppNode
inspectLoanWidget AppModel{lendingModel=LendingModel{..},scene=_,..} = do
    zstack
      [ vstack
          [ vstack
              [ centerWidgetH $
                  label "Loan ID"
                    `styleBasic` [textFont "Italics", textColor customBlue]
              , spacer
              , centerWidgetH $ hstack
                  [ copyableLabelSelf (display targetId) lightGray 12
                  , spacer_ [width 3]
                  , tooltip_ "Resync History" [tooltipDelay 0] $
                      box_ [alignMiddle, onClick $ LendingEvent $ LookupLoanHistory $ StartProcess $ Just targetId] $
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
              , widgetMaybe mLoanUTxO $ \loanUTxO ->
                  vstack
                    [ spacer
                    , hstack
                        [ label "Current Loan Status:"
                            `styleBasic` [textSize 12]
                        , spacer_ [width 10]
                        , if isExpired then
                            label "Expired"
                              `styleBasic` [textColor customRed, textSize 12]
                          else 
                            label "Active"
                              `styleBasic` [textColor customBlue, textSize 12]
                        ]
                    , spacer_ [width 5]
                    , hstack
                        [ spacer
                        , activeStatus loanUTxO
                        ]
                    ]
              , widgetIf (isNothing mLoanUTxO) $
                  vstack
                    [ spacer
                    , hstack
                        [ label "Current Loan Status:"
                            `styleBasic` [textSize 12]
                        , spacer_ [width 10]
                        , label "Finished"
                            `styleBasic` [textColor customBlue, textSize 12]
                        ]
                    ]
              , spacer
              , label "Event History"
                  `styleBasic` [textSize 12]
              , spacer_ [width 5]
              , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
                  vstack_ [childSpacing] (map (hstack . (spacer:) . pure . eventRow) history)
              , filler
              , hstack
                  [ filler
                  , button "Close" $ HomeEvent CloseInspectedCorrespondingLoan
                  , spacer
                  , tooltip_ actionTip [tooltipDelay 0] $ mainButton actionIcon actionEvt
                      `styleBasic` 
                        [ textFont "Remix"
                        , textMiddle
                        ]
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
      , updatePaymentAddressWidget `nodeVisible`
          isJust (homeModel ^. #newLenderAddressUpdate)
      ]
  where
    targetId :: Loans.LoanId
    targetId = fromMaybe "" $ homeModel ^. #inspectedLoan

    (history, mLoanUTxO) = fromMaybe ([],Nothing) 
                         $ Map.lookup targetId cachedLoanHistories

    mCurrentDatum = mLoanUTxO >>= loanUTxOActiveDatum

    LoanEvent{state=mPreviousDatum} = fromMaybe def $ maybeLast history

    -- Only the constant terms are needed.
    activeDatum = fromMaybe def $ mPreviousDatum <|> mCurrentDatum

    isExpired = activeDatum ^. #loanExpiration <= toPlutusTime (config ^. #currentTime)

    (actionEvt, actionTip, actionIcon)
      | isNothing mLoanUTxO =
          ( HomeEvent $ BurnLoanKeyNFT $ activeDatum ^. #loanId
          , "Burn Key NFT for finished loan"
          , burnIcon
          )
      | toPlutusTime (config ^. #currentTime) >= activeDatum ^. #loanExpiration =
          ( HomeEvent $ ClaimExpiredCollateral $ fromMaybe def mLoanUTxO
          , "Claim collateral from defaulted loan"
          , claimCollateralIcon
          )
      | otherwise =
          ( HomeEvent $ UpdateLenderPaymentAddress $ StartAdding mLoanUTxO
          , "Update loan payment address"
          , updatePaymentAddressIcon
          )

    explainEvent :: LoanEvent -> Text
    explainEvent LoanEvent{event, timeStamp} =
      let loanNativeAsset = toNativeAsset $ activeDatum ^. #loanAsset
       in case event of
            Left (Loans.CreateActive _) -> "Loan started."
            Right (Loans.MakePayment amount) -> 
              let finalStmt
                    | toRational amount >= toRational (activeDatum ^. #loanOutstanding) = "final "
                    | otherwise = ""
               in mconcat
                    [ "Made " <> finalStmt <> "payment of "
                    , showAssetBalance True reverseTickerMap $ loanNativeAsset & #quantity .~ amount
                    , "."
                    ]
            Right (Loans.ApplyInterest deposit times) -> mconcat
              [ "Applied interest "
              , show times
              , " time(s) with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.SpendWithKeyNFT -> "Defaulted collateral claimed by lender."
            Right (Loans.UpdateLenderAddress newAddress deposit) -> mconcat
              [ "Lender changed the required payment address to "
              , display $ either (const "") fst $
                  plutusToBech32 (config ^. #network) newAddress
              , " with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.Unlock -> 
              if activeDatum ^. #claimExpiration < toPlutusTime timeStamp 
              then "Lost collateral claimed by borrower."
              else "Invalid Active UTxO closed by borrower."
            _ -> error "Other loan redeemer used."

    eventRow :: LoanEvent -> AppNode
    eventRow e@LoanEvent{timeStamp} = do
      vstack
        [ hstack
            [ label (explainEvent e)
                `styleBasic` [textSize 10]
            , filler
            ]
        , spacer_ [width 2]
        , hstack
            [ label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) timeStamp)
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
            , label (showLocalTime (config ^. #timeZone) timeStamp)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

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

updatePaymentAddressWidget :: AppNode
updatePaymentAddressWidget = do
  let maybeLens' = maybeLens def (#homeModel % #newLenderAddressUpdate)
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label "Where would you like future loan payments to go?"
        , spacer_ [width 20]
        , hstack
            [ label "Address:"
            , spacer
            , textField (toLensVL $ maybeLens' % #newPaymentAddress)
                `styleBasic` [textSize 10, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ HomeEvent $ UpdateLenderPaymentAddress CancelAdding
            , spacer
            , mainButton "Confirm" $ HomeEvent $ UpdateLenderPaymentAddress ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Color -> Double -> WidgetNode s AppEvent
copyableLabelSelf caption color fontSize = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor color
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
