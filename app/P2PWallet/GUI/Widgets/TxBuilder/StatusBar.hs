module P2PWallet.GUI.Widgets.TxBuilder.StatusBar
  ( 
    statusBar
  , changeInfoPopup
  , collateralInfoPopup
  ) where

import Monomer hiding 
  ( popupAnchor
  , alignTop
  , popupAlignToOuterV
  , popupAlignToOuterH
  )
import Prettyprinter (pretty, align, vsep)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Prelude

statusBar :: AppModel -> AppNode
statusBar AppModel{txBuilderModel=TxBuilderModel{..}} = do
  let checkboxIcon b
        | isNothing b = indeterminateCheckboxIcon
        | b == Just True = checkedBoxIcon
        | otherwise = uncheckedBoxIcon
      checkboxColor b
        | isNothing b = gray
        | b == Just True = customBlue
        | otherwise = customRed
      collateralState
        | requiresCollateral = Just $ isJust collateralInput
        | otherwise = Nothing
      changeAddressSet = maybe False (("" /=) . view #paymentAddress) changeOutput
      hasInputs = or
        [ userInputs /= []
        , swapBuilderModel ^. #swapCloses /= []
        ]
  hstack_ [childSpacing]
    [ hstack
        [ label "Change Address"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon $ Just changeAddressSet)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor $ Just changeAddressSet
              , textMiddle
              , textSize 12
              ]
        ]
    , separatorLine
    , hstack
        [ label "Inputs"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon $ Just hasInputs)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor $ Just hasInputs
              , textMiddle
              , textSize 12
              ]
        ]
    , separatorLine
    , hstack
        [ label "Balanced"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon $ Just isBalanced)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor $ Just isBalanced
              , textMiddle
              , textSize 12
              ]
        ]
    , separatorLine
    , hstack
        [ label "Collateral"
            `styleBasic`
              [ textSize 12 ]
        , box_ [onClick $ Alert aboutCollateralMsg] $
            label helpIcon
              `styleBasic`
                [ padding 2
                , textSize 8
                , border 0 transparent
                , radius 20
                , textMiddle
                , bgColor transparent
                , textColor customBlue
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 3]
        , label (checkboxIcon collateralState)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor collateralState
              , textMiddle
              , textSize 12
              ]
        ]
    ] `styleBasic`
        [ bgColor customGray2
        , border 1 black
        , padding 10
        , radius 15
        ]

changeInfoPopup :: AppModel -> AppNode
changeInfoPopup AppModel{txBuilderModel,reverseTickerMap} = do
  let ChangeOutput{..} = fromMaybe def $ txBuilderModel ^. #changeOutput
      prettyAssets = map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
      anchor = 
        box_ [alignMiddle] $ tooltip_ "Change Info" [tooltipDelay 0] $
          button changeIcon (TxBuilderEvent ShowTxChangePopup)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 2
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
  customPopup_ (toLensVL $ #txBuilderModel % #showChangePopup) 
    [popupAnchor anchor, alignBottom, popupAlignToOuterV] $
    vstack
      [ hstack 
          [ label "Change Address:"
              `styleBasic` [textSize 8]
          , spacer
          , if paymentAddress == "" then 
              label "not set"
               `styleBasic` [textColor customRed, textSize 8]
            else
              label (toText paymentAddress)
               `styleBasic` [textSize 8]
          ]
      , spacer_ [width 5]
      , hstack 
          [ label "Value:"
              `styleBasic` [textSize 8]
          , spacer
          , label (display lovelace)
              `styleBasic` [textSize 8]
          ]
      , widgetIf (not $ null nativeAssets) $
          vstack
            [ spacer_ [width 5]
            , label "Native Assets:" `styleBasic` [textSize 8]
            , hstack
                [ spacer_ [width 10]
                , vstack
                    [ spacer_ [width 10]
                    , copyableTextArea (show $ align $ vsep prettyAssets)
                        `styleBasic` 
                          [ height $ 20 + 12 * (fromIntegral (length nativeAssets) - 1)
                          , textSize 8
                          , maxWidth 300
                          ]
                    ]
                ]
            ]
      ] `styleBasic`
          [ bgColor customGray3
          , border 1 black
          , padding 10
          , maxWidth 600
          ]

collateralInfoPopup :: AppModel -> AppNode
collateralInfoPopup AppModel{txBuilderModel} = do
  let CollateralInput{..} = fromMaybe def $ txBuilderModel ^. #collateralInput
      anchor = 
        box_ [alignMiddle] $ tooltip_ "Collateral Info" [tooltipDelay 0] $
          button collateralIcon (TxBuilderEvent ShowTxCollateralPopup)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 2
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              , textSize 14
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
  customPopup_ (toLensVL $ #txBuilderModel % #showCollateralPopup) 
    [popupAnchor anchor, alignBottom, popupAlignToOuterV] $
    vstack
      [ hstack 
          [ label "Output Reference:"
              `styleBasic` [textSize 8]
          , spacer
          , label (display utxoRef)
               `styleBasic` [textSize 8]
          ]
      , spacer_ [width 5]
      , hstack 
          [ label "From Wallet:"
              `styleBasic` [textSize 8]
          , spacer
          , label walletAlias
              `styleBasic` [textSize 8]
          ]
      , spacer_ [width 5]
      , hstack 
          [ label "Payment Address:"
              `styleBasic` [textSize 8]
          , spacer
          , label (display paymentAddress)
              `styleBasic` [textSize 8]
          ]
      , spacer_ [width 5]
      , hstack 
          [ label "Value:"
              `styleBasic` [textSize 8]
          , spacer
          , label (display lovelace)
              `styleBasic` [textSize 8]
          ]
      ] `styleBasic`
          [ bgColor customGray3
          , border 1 black
          , padding 10
          , maxWidth 600
          ]
