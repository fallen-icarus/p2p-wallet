module P2PWallet.GUI.Widgets.Home.NativeAssets.OptionsKeys
  ( 
    inspectOptionsContractWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

inspectOptionsContractWidget :: AppModel -> AppNode
inspectOptionsContractWidget AppModel{optionsModel=OptionsModel{..},scene=_,..} = do
      vstack
        [ vstack
            [ centerWidgetH $
                label "Contract ID"
                  `styleBasic` [textFont "Italics", textColor customBlue]
            , spacer
            , centerWidgetH $ hstack
                [ copyableLabelSelf (display targetId) lightGray 12
                , spacer_ [width 3]
                , tooltip_ "Resync State" [tooltipDelay 0] $
                    let evt = OptionsEvent $ LookupOptionsContract $ StartProcess $ Just targetId
                     in box_ [alignMiddle, onClick evt] $
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
            , widgetIf (isJust mOptionsUTxO) $
                vstack
                  [ spacer
                  , hstack
                      [ label "Contract State:"
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
                      , activeStatus
                      ]
                  ]
            , widgetIf (isNothing mOptionsUTxO) $
                centerWidget $ vstack
                  [ spacer
                  , hstack
                      [ label "Current State:"
                          `styleBasic` [textSize 14]
                      , spacer_ [width 10]
                      , label "Expired and Closed"
                          `styleBasic` [textColor customRed, textSize 14]
                      ]
                  ]
            , filler
            , hstack
                [ filler
                , button "Close" $ HomeEvent CloseInspectedCorrespondingOptionsContract
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
  where
    targetId :: Options.ContractId
    targetId = fromMaybe "" $ homeModel ^. #inspectedOptionsContract

    Config{network} = config

    mOptionsUTxO = fromMaybe Nothing
                 $ Map.lookup targetId cachedKeyContracts

    optionsUTxO@OptionsUTxO{utxoRef} = fromMaybe def mOptionsUTxO

    Options.ActiveDatum{..} = fromMaybe def $ mOptionsUTxO >>= optionsUTxOActiveDatum

    isExpired = expiration <= toPlutusTime (config ^. #currentTime)

    (actionEvt, actionTip, actionIcon)
      | isExpired =
          ( HomeEvent $ BurnOptionsKeyNFT targetId
          , "Burn Key NFT for finished options contract"
          , burnIcon
          )
      | otherwise =
          ( HomeEvent $ ExecuteOptionsContract optionsUTxO
          , "Execute Contract"
          , swapIcon
          )

    activeStatus :: AppNode
    activeStatus = do
      let offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords
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
