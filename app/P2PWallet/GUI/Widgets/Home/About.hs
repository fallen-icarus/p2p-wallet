module P2PWallet.GUI.Widgets.Home.About 
  ( 
    aboutWidget
  ) where

import Monomer
import Prettyprinter (pretty)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

aboutWidget :: AppModel -> AppNode
aboutWidget model = do
    vstack 
      [ hstack [spacer, addressInfoWidget model, spacer]
      , filler
      , hstack
          [ filler
          -- The widget is initialized using PairPaymentWallet. It is the same for all wallet 
          -- types.
          , box (mainButton "Add Wallet" $ HomeEvent $ PairPaymentWallet StartAdding) 
              `styleBasic` [padding 20]
          ]
      ] `nodeVisible` (not isAdding)
  where
    isAdding :: Bool
    isAdding = model ^. #homeModel % #addingWallet

-- | Show key information about the address.
addressInfoWidget :: AppModel -> AppNode
addressInfoWidget model = do
  let wallet = model ^. #homeModel % #selectedWallet
      hasStaking = isJust $ wallet ^. #stakeAddress
      addrInfo = inspectBech32Address $ wallet ^. #paymentAddress % #unPaymentAddress
      spendingKeyHash = fromMaybe "" $ either (const Nothing) infoSpendingKeyHash addrInfo
      stakeKeyHash = either (const Nothing) infoStakeKeyHash addrInfo

  vstack 
    [ centerWidgetH $ label "Address Info" 
        `styleBasic` [textFont "Italics", textSize 18, padding 10]
    , vstack
        [ hstack
            [ spacer_ [width 10]
            , label "Payment Credential" `styleBasic` [textColor lightGray, textFont "Italics"]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelWith "Address:" showAddressFormatted $ wallet ^. #paymentAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer_ [width 20]
            , copyableLabelFor "Key Hash:" $ show $ PubKeyHash $ BuiltinByteString spendingKeyHash
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Derivation Path:" $ fromMaybe "none" $
                show . pretty . showDerivationPath <$> wallet ^. #paymentKeyPath
            ] `styleBasic` [padding 5]
              `nodeVisible` (isJust $ wallet ^. #paymentKeyPath)
        ]
    , spacer
    , separatorLine `styleBasic` [paddingL 20, paddingR 20]
    , spacer
    , vstack
        [ hstack
            [ spacer_ [width 10]
            , label "Stake Credential" `styleBasic` [textColor lightGray, textFont "Italics"]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Address:" $ maybe "none" toText $ wallet ^. #stakeAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer_ [width 20]
            , copyableLabelFor "Key Hash:" $ 
                fromMaybe "" $ show . PubKeyHash . BuiltinByteString <$> stakeKeyHash
            ] `styleBasic` [padding 5]
              `nodeVisible` hasStaking
        , hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Derivation Path:" $ fromMaybe "" $
                show . pretty . showDerivationPath <$> wallet ^. #stakeKeyPath
            ] `styleBasic` [padding 5]
              `nodeVisible` hasStaking
        ]
    ] `styleBasic`
        [ padding 10
        , radius 20
        , bgColor customGray3
        ]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy other data.
copyableLabelFor :: Text -> Text -> WidgetNode s AppEvent
copyableLabelFor caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText info)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label info `styleBasic` [textColor lightGray]
    ]

copyableLabelWith :: (ToText a) => Text -> (a -> Text) -> a -> WidgetNode s AppEvent
copyableLabelWith caption modifier fullInfo = do
  let formattedInfo = modifier fullInfo
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText $ toText fullInfo)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label formattedInfo `styleBasic` [textColor lightGray]
    ]
