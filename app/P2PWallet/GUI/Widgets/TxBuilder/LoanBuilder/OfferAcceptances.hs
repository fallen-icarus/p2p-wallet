module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferAcceptances
  ( 
    offerAcceptancesList
  , editOfferAcceptanceWidget
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
import P2PWallet.Plutus
import P2PWallet.Prelude

offerAcceptancesList :: ReverseTickerMap -> [(Int,OfferAcceptance)] -> [AppNode]
offerAcceptancesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,OfferAcceptance) -> AppNode
    utxoRow s@(idx,OfferAcceptance{..}) = do
      let Loans.OfferDatum{..} = fromMaybe def $ loanUTxOOfferDatum offerUTxO
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          prettyLoanAmount = showAssetBalance True reverseTickerMap loanAmount
          numberOfCollateral = length $ filter ((> 0) . view #quantity) collateralAmounts
          prettyDuration = show $ calcDaysInPosixPeriod $ fromPlutusTime loanTerm
      hstack
        [ vstack
            [ hstack
                [ label ("Accept Offer for " <> prettyLoanAmount <> " Loan")
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label ("Duration: " <> prettyDuration <> " Days")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Borrower ID: " <> alias <> " (" <> display borrowerCredential <> ")")
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label (show numberOfCollateral <> " Collateral Asset(s)")
                    `styleBasic` [textSize 8, textColor lightGray]
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
                    (loanBuilderEvent $ EditSelectedOfferAcceptance $ StartAdding $ Just s)
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
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferAcceptance idx)
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

editOfferAcceptanceWidget :: AppModel -> AppNode
editOfferAcceptanceWidget AppModel{..} = do
    let maybeLens' = maybeLens (def,def) 
                   $ #txBuilderModel % #loanBuilderModel % #targetOfferAcceptance
        NewOfferAcceptance{offerUTxO} = maybe def snd $ 
          txBuilderModel ^. #loanBuilderModel % #targetOfferAcceptance
        Loans.OfferDatum{loanAsset,collateralization,loanPrincipal} = fromMaybe def $
          loanUTxOOfferDatum offerUTxO
        loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
        collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                         $ collateralization ^. #unCollateralization
    centerWidget $ vstack
      [ centerWidgetH $
          label "Edit your specified collateral"
            `styleBasic` [textFont "Italics", textColor customBlue]
      , label ("Loan Amount: " <> showAssetBalance True reverseTickerMap loanAmount)
          `styleBasic` [textSize 12]
      , spacer
      , label "Collateral Rates:"
          `styleBasic` [textSize 12]
      , spacer_ [width 3]
      , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
          \col -> hstack_ [childSpacing_ 3] $ map (collateralAssetWidget loanAmount) col
      , hstack
          [ label "Collateral Assets (separated with newlines)"
              `styleBasic` [textSize 12]
          , mainButton helpIcon (Alert collateralAmountsMsg)
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
      , textArea (toLensVL $ maybeLens' % _2 % #collateralAmounts)
          `styleBasic` [height 180, textSize 10, bgColor customGray1]
          `styleFocus` [border 1 customBlue]
      , spacer
      , box_ [alignRight] $ 
          hstack
            [ button "Cancel" (loanBuilderEvent $ EditSelectedOfferAcceptance CancelAdding)
                `styleBasic` [textSize 10]
            , spacer
            , mainButton "Confirm" (loanBuilderEvent $ EditSelectedOfferAcceptance ConfirmAdding)
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
            `styleBasic` [textSize 12, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]
