module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalUpdates
  ( 
    proposalUpdatesList
  , editProposalUpdateWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

proposalUpdatesList :: ReverseTickerMap -> TimeZone -> [(Int,ProposalUpdate)] -> [AppNode]
proposalUpdatesList reverseTickerMap timeZone = map utxoRow
  where
    termsRow 
      :: NativeAsset 
      -> NativeAsset 
      -> (NativeAsset,Rational, POSIXTime) 
      -> AppNode
    termsRow offerAsset askAsset (premium, strikePrice, expiration) = do
      let formattedPrice = showPriceFormatted reverseTickerMap askAsset offerAsset strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
      hstack
        [ spacer_ [width 2]
        , label ("Premium: " <> showAssetBalance True reverseTickerMap premium)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Price: " <> prettyPrice)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Expiration: " <> showLocalDate timeZone (expiration - 1))
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    utxoRow :: (Int,ProposalUpdate) -> AppNode
    utxoRow s@(idx,ProposalUpdate{oldProposal,newProposal=ProposalCreation{..}}) = do
      hstack
        [ vstack
            [ hstack
                [ label ("Update Options Proposal for " <> alias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , let prettyRef = display (oldProposal ^. #utxoRef) in
                  flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText prettyRef] $
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
                , filler
                , label (showAssetBalance True reverseTickerMap offerAsset)
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 2]
                , label remixArrowRightFill
                    `styleBasic` [textMiddle, textFont "Remix", textSize 10, textColor white]
                , spacer_ [width 2]
                , label (showAssetNameOnly reverseTickerMap askAsset)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label "Possible Terms:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , hstack
                [ spacer
                , vstack (map (termsRow offerAsset askAsset) possibleTerms)
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
                    (optionsBuilderEvent $ EditSelectedProposalUpdate $ StartAdding $ Just s)
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
                button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedProposalUpdate idx)
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

editProposalUpdateWidget :: AppModel -> AppNode
editProposalUpdateWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens (0,def) $ #txBuilderModel % #optionsBuilderModel % #targetProposalUpdate
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
  centerWidget $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "Edit Options Proposal Update"
          `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
    , spacer
    , hstack
        [ helpButton proposalOfferAmountMsg
        , spacer_ [width 3]
        , label "Offer Amount:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #offerAsset) 
              [placeholder "10 ADA"] 
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ helpButton proposalAskAssetMsg
        , spacer_ [width 3]
        , label "Ask Asset:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #askAsset) 
              [placeholder "DJED"] 
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ helpButton proposalPremiumAssetMsg
        , spacer_ [width 3]
        , label "Premium Asset:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #premiumAsset) 
              [placeholder "DJED"] 
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ helpButton proposalPaymentAddressMsg
        , spacer_ [width 3]
        , label "Payment Address:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #paymentWallet) 
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
        ]
    , spacer
    , separatorLine `styleBasic` [fgColor darkGray]
    , spacer
    , hstack
        [ helpButton proposalTermsMsg
        , spacer_ [width 3]
        , label "Possible Terms (separated with newlines):"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #possibleTerms)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (optionsBuilderEvent $ EditSelectedProposalUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" (optionsBuilderEvent $ EditSelectedProposalUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]
