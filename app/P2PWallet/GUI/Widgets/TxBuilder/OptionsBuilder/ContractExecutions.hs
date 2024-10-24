module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ContractExecutions
  ( 
    optionsContractExecutionsList
  ) where

import Monomer as M

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

optionsContractExecutionsList 
  :: ReverseTickerMap 
  -> TimeZone 
  -> [(Int,OptionsContractExecution)] 
  -> [AppNode]
optionsContractExecutionsList reverseTickerMap timeZone = map utxoRow
  where
    utxoRow :: (Int,OptionsContractExecution) -> AppNode
    utxoRow (idx,OptionsContractExecution{..}) = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum optionsUTxO
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          formattedPrice = showPriceFormatted reverseTickerMap askValue offerAmount 
                         $ toRational strikePrice
          prettyPrice = mconcat
            [ "Strike Price: "
            , formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askValue
            , " / "
            , showAssetNameOnly reverseTickerMap offerAmount
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords
            [ "Payments to:"
            , display payToAddress
            ]
          prettyExpirationTime = unwords
            [ "Expiration:"
            , showLocalDate timeZone $ fromPlutusTime expiration
            , showLocalTime timeZone $ fromPlutusTime expiration
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Execute Options Contract"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display $ optionsUTxO ^. #utxoRef in
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
                , flip styleBasic [textSize 10] $ tooltip_ walletAlias [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
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
                , flip styleBasic [textSize 10] $ 
                    tooltip_ ("Contract ID: " <> display contractId) [tooltipDelay 0] $
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
                , label (showAssetBalance True reverseTickerMap offerAmount)
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 2]
                , label remixArrowRightFill
                    `styleBasic` [textMiddle, textFont "Remix", textSize 10, textColor white]
                , spacer_ [width 2]
                , label (showAssetBalance True reverseTickerMap askValue)
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
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedOptionsContractExecution idx)
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

