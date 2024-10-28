module P2PWallet.GUI.Widgets.TxBuilder.TestMint
  ( 
    testMintRow
  , addTestMintWidget
  ) where

import Monomer
import Data.Text qualified as Text
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.TxBuilder.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

testMintRow :: ReverseTickerMap -> TestMint -> AppNode
testMintRow reverseTickerMap TestMint{..} = do
    hstack
      [ vstack
          [ hstack
              [ label "Mint/Burn Test Tokens" `styleBasic` [textSize 10, textColor customBlue]
              , filler
              , copyableLabelSelf (show alwaysSucceedPolicyHash) white 10
              ] 
          , spacer_ [width 2]
          , vstack_ [childSpacing_ 3] $ for (groupInto 2 $ testMintToNativeAssets mint) $ 
              \assetRow -> 
                hstack_ [childSpacing_ 3] $ [filler] <> map assetMintWidget assetRow
          ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]
      , spacer_ [width 3]
      , box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
          button editIcon 
              (TxBuilderEvent $ AddNewTestMint $ StartAdding Nothing)
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
          button closeCircleIcon (TxBuilderEvent RemoveTestMint)
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
  where
    assetMintWidget :: NativeAsset -> AppNode
    assetMintWidget NativeAsset{..} = do
      let (fluxIcon,color)
            | quantity < 0 = (remixSubtractLine, customRed)
            | otherwise = (remixAddLine, customBlue)
          (name,formattedQuantity) = case Map.lookup (policyId,tokenName) reverseTickerMap of
            Nothing -> (display fingerprint, show quantity)
            Just (tckr,decimal) -> (display tckr, show $ formatQuantity decimal quantity)
      hstack
        [ label fluxIcon 
            `styleBasic` 
              [ textFont "Remix"
              , textSize 8
              , bgColor color
              , padding 1
              , radius 20
              , textMiddle
              , textCenter
              ]
        , spacer_ [width 3]
        , copyableLabelSelf name lightGray 8
        , spacer_ [width 3]
        , label formattedQuantity
            `styleBasic` 
              [ textSize 8, padding 3, radius 3, bgColor customGray3, textColor color]
        , spacer_ [width 2]
        ] `styleBasic` [bgColor customGray4, paddingT 2, paddingB 2, paddingL 2, paddingR 0]

addTestMintWidget :: AppModel -> AppNode
addTestMintWidget AppModel{txBuilderModel=TxBuilderModel{newTestMint=NewTestMint{..}}} = do
  centerWidget $ vstack 
    [ centerWidgetH $ label "Which tokens would you like to mint/burn?"
    , spacer
    -- A converter from human-readable text to hexidecimal.
    , hstack
        [ label "Converter:" `styleBasic` [textSize 14]
        , spacer_ [width 10]
        , textField_ (toLensVL $ #txBuilderModel % #newTestMint % #exampleInput)
              [placeholder "TestToken1"]
            `styleBasic` [bgColor customGray1, width 200, textSize 10, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 10]
        , tooltip_ "Convert" [tooltipDelay 0] $
            box_ [alignMiddle, onClick $ TxBuilderEvent ConvertExampleTestMintNameToHexidecimal] $
              label remixArrowRightLine 
                `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , spacer_ [width 10]
        , tooltip_ "Copy" [tooltipDelay 0] $ box_ [alignMiddle, onClick (CopyText exampleOutput)] $
            label_ exampleOutput [ellipsis]
              `styleBasic`
                [ padding 10
                , radius 5
                , textLeft
                , textSize 10
                , border 1 black
                , textColor white
                , bgColor customGray2
                , width 200
                ]
              `styleHover` [textColor customBlue, cursorIcon CursorHand]
        ]
    , widgetIf (Text.length exampleOutput > 64) $
        label "Hexidecimal token names must be less than 64 characters."
          `styleBasic`
              [ textSize 8
              , textColor customRed
              ]
    , spacer
    , hstack
        [ label "Token Quantities (separated with newlines):"
            `styleBasic` [textSize 14]
        , mainButton helpIcon (Alert testTokenMintQuantitiesMsg)
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
    , textArea (toLensVL $ #txBuilderModel % #newTestMint % #mint)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ AddNewTestMint CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ AddNewTestMint ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, radius 10, paddingB 5]

