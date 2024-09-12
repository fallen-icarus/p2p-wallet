module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalPurchases
  ( 
    proposalPurchasesList
  ) where

import Monomer as M
import Data.List ((!!))

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

proposalPurchasesList :: ReverseTickerMap -> TimeZone -> [(Int,ProposalPurchase)] -> [AppNode]
proposalPurchasesList reverseTickerMap timeZone = map utxoRow
  where
    termsRow 
      :: (Int,ProposalPurchase)
      -> NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> (Integer,Options.Terms)
      -> AppNode
    termsRow targetPurchase offerAsset askAsset premiumAsset (idx,terms) = do
      let (_, ProposalPurchase{desiredTerms}) = targetPurchase
          Options.Terms{premium,expiration,strikePrice} = terms
          formattedPrice = 
            showPriceFormatted reverseTickerMap askAsset offerAsset $ toRational strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
          prettyPremium = 
            showAssetBalance True reverseTickerMap $ premiumAsset & #quantity .~ premium
          editEvt = optionsBuilderEvent 
                  $ EditSelectedProposalPurchase 
                  $ StartProcess 
                  $ Just
                  $ targetPurchase & _2 % #desiredTerms .~ idx
          termsWidget = hstack
            [ spacer_ [width 2]
            , label ("Premium: " <> prettyPremium)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Price: " <> prettyPrice)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Expiration: " <> showLocalDate timeZone (fromPlutusTime $ expiration - 1))
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            ] `styleBasic` 
                [ bgColor customGray4
                , padding 2
                , radius 3
                , border 1 $ if desiredTerms == idx then customBlue else customGray1
                ]
              `styleHover` [border 1 customBlue, configIf (desiredTerms /= idx) $ cursorIcon CursorHand]
      if desiredTerms == idx then termsWidget else
        tooltip_ "Change Selected Terms" [tooltipDelay 0] $ 
          box_ [onClick editEvt] termsWidget

    utxoRow :: (Int,ProposalPurchase) -> AppNode
    utxoRow s@(idx,ProposalPurchase{..}) = do
      let Options.ProposalDatum{..} = fromMaybe def $ optionsUTxOProposalDatum proposalUTxO
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          premiumNativeAsset = toNativeAsset premiumAsset
          Options.Terms{strikePrice} = possibleTerms !! fromIntegral desiredTerms
          askQuantity = roundUp $ toRational strikePrice * toRational offerQuantity
          askNativeAsset = toNativeAsset askAsset & #quantity .~ askQuantity
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords
            [ "Payments to:"
            , display payToAddress
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Purchase Options Proposal"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , let prettyRef = display (proposalUTxO ^. #utxoRef) in
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
                , label (showAssetBalance True reverseTickerMap offerAmount)
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 2]
                , label remixArrowRightFill
                    `styleBasic` [textMiddle, textFont "Remix", textSize 10, textColor white]
                , spacer_ [width 2]
                , label (showAssetBalance True reverseTickerMap askNativeAsset)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label "Possible Terms:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , hstack
                [ spacer
                , vstack $ 
                    map (termsRow s offerAmount askNativeAsset premiumNativeAsset) $ 
                      zip [0..] possibleTerms
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedProposalPurchase idx)
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

