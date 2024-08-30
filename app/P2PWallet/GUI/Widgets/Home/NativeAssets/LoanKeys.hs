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
                  label "Event History For Loan ID"
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
              , spacer
              , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
                  vstack_ [childSpacing] (map eventRow history)
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
          (isJust $ homeModel ^. #newLenderAddressUpdate)
      ]
  where
    targetId :: Loans.LoanId
    targetId = fromMaybe "" $ homeModel ^. #inspectedLoan

    (history, mLoanUTxO) = fromMaybe ([],Nothing) 
                         $ Map.lookup targetId cachedLoanHistories
    mCurrentDatum = mLoanUTxO >>= loanUTxOActiveDatum

    LoanEvent{state=mPreviousDatum} = fromMaybe def $ maybeLast history

    -- Only the constant terms are needed.
    Loans.ActiveDatum{..} = fromMaybe def $ mPreviousDatum <|> mCurrentDatum

    (actionEvt, actionTip, actionIcon)
      | isNothing mLoanUTxO =
          ( HomeEvent $ BurnLoanKeyNFT loanId
          , "Burn Key NFT for finished loan"
          , burnIcon
          )
      | toPlutusTime (config ^. #currentTime) >= loanExpiration =
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
      let loanNativeAsset = toNativeAsset loanAsset
       in case event of
            Left (Loans.CreateActive _) -> "Loan started."
            Right (Loans.MakePayment amount) -> mconcat
              [ "Made payment of "
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
              , display $ fromRight "" $ 
                  fmap fst $ plutusToBech32 (config ^. #network) newAddress
              , " with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.Unlock -> 
              if claimExpiration < toPlutusTime timeStamp 
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

updatePaymentAddressWidget :: AppNode
updatePaymentAddressWidget = do
  let maybeLens' = maybeLens def (#homeModel % #newLenderAddressUpdate)
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label ("Where would you like future loan payments to go?")
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
