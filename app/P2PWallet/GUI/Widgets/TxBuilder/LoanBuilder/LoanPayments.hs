module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.LoanPayments
  ( 
    loanPaymentsList
  , editLoanPaymentWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

loanPaymentsList :: ReverseTickerMap -> [(Int,LoanPayment)] -> [AppNode]
loanPaymentsList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,LoanPayment) -> AppNode
    utxoRow s@(idx,LoanPayment{..}) = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum activeUTxO
          prettyPayment = showAssetBalance True reverseTickerMap paymentAmount
          paymentType
            | isFullPayment = "Full"
            | otherwise = "Partial"
      hstack
        [ vstack
            [ hstack
                [ label ("Make " <> paymentType <> " Payment of " <> prettyPayment)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label ("For Borrower " <> alias)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Loan ID: " <> display loanId)
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , hstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (loanBuilderEvent $ EditSelectedLoanPayment $ StartAdding $ Just s)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedLoanPayment idx)
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
        ]

editLoanPaymentWidget :: AppModel -> AppNode
editLoanPaymentWidget AppModel{..} = do
    let maybeLens' = maybeLens (def,def) $ #txBuilderModel % #loanBuilderModel % #targetLoanPayment
        NewLoanPayment{activeUTxO,paymentAmount} = maybe def snd $ 
          txBuilderModel ^. #loanBuilderModel % #targetLoanPayment
        Loans.ActiveDatum{loanAsset,collateralization,loanOutstanding} = fromMaybe def $
          loanUTxOActiveDatum activeUTxO
        loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
        collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                         $ collateralization ^. #unCollateralization
        mPaymentAmount = toRational . view #quantity <$> 
          rightToMaybe (parseNativeAssets tickerMap mempty paymentAmount)
        startingBalance = toRational $ roundUp $ toRational loanOutstanding
        mPaymentRatio = (/ startingBalance) <$> mPaymentAmount
        prettyRatio = (<> "%") $ show @_ @Decimal $ (*100) $ realFracToDecimal 10
                    $ fromMaybe 0 mPaymentRatio
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
    centerWidget $ vstack
      [ centerWidgetH $
          label "New Loan Payment"
            `styleBasic` [textFont "Italics", textColor customBlue]
      , spacer
      , label ("Current Balance: " <> showAssetBalance True reverseTickerMap loanBalance)
          `styleBasic` [textSize 12]
      , spacer
      , hstack
          [ label "Payment Amount:"
              `styleBasic` [textSize 12]
          , spacer
          , textField_ (toLensVL $ maybeLens' % _2 % #paymentAmount) 
                [placeholder $ showAssetBalance True reverseTickerMap loanBalance]
              `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
              `styleFocus` [border 1 customBlue]
          , spacer_ [width 3]
          , helpButton offerLoanAmountMsg
          , spacer_ [width 7]
          , box_ [alignMiddle, onClick $ loanBuilderEvent SetEditLoanPaymentToFullPayment] $
              tooltip_ "Pay remaining balance" [tooltipDelay 0] $
                (label "max" `styleBasic` [padding 5, textSize 10])
                `styleBasic` [radius 5, padding 5, bgColor customGray1]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
          , filler
          , label ("Collateral Unlocked: " <> prettyRatio)
              `styleBasic` [textSize 12]
          ]
      , spacer
      , label "Collateral Rates:"
          `styleBasic` [textSize 12]
      , spacer
      , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
          \col -> hstack_ [childSpacing_ 3] $ [spacer] <> map (collateralAssetWidget loanBalance) col
      , spacer
      , hstack
          [ label "Collateral Assets (separated with newlines)"
              `styleBasic` [textSize 12]
          , spacer_ [width 3]
          , helpButton paymentCollateralAmountsMsg
          ]
      , spacer
      , textArea (toLensVL $ maybeLens' % _2 % #collateralBalances)
          `styleBasic` [height 180, textSize 10, bgColor customGray1]
          `styleFocus` [border 1 customBlue]
      , spacer
      , box_ [alignRight] $ 
          hstack
            [ button "Cancel" (loanBuilderEvent $ EditSelectedLoanPayment CancelAdding)
                `styleBasic` [textSize 10]
            , spacer
            , mainButton "Confirm" (loanBuilderEvent $ EditSelectedLoanPayment ConfirmAdding)
                `styleBasic` [textSize 10]
            ]
      , spacer
      ] `styleBasic`
          [ bgColor customGray3
          , radius 10
          , border 1 black
          , padding 30
          , paddingB 0
          ]
  where
    collateralAssetWidget :: NativeAsset -> (NativeAsset,Rational) -> AppNode
    collateralAssetWidget loanAsset (collateralAsset, price) = do
      let formattedPrice = showPriceFormatted reverseTickerMap collateralAsset loanAsset price
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap collateralAsset
            , " / "
            , showAssetNameOnly reverseTickerMap loanAsset
            ]
      hstack
        [ spacer_ [width 2]
        , label prettyPrice
            `styleBasic` [textSize 10, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4 , padding 2 , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]
