module P2PWallet.GUI.Widgets.Home.About 
  ( 
    aboutWidget
  ) where

import Monomer
import Prettyprinter (pretty,tupled)
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
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
          , box (mainButton "Add Wallet" $ HomeEvent $ PairPaymentWallet $ StartAdding Nothing) 
              `styleBasic` [padding 20]
          ]
      ] `nodeVisible` not isAdding
  where
    isAdding :: Bool
    isAdding = model ^. #homeModel % #addingWallet

-- Show key information about the address.
addressInfoWidget :: AppModel -> AppNode
addressInfoWidget AppModel{homeModel=HomeModel{selectedWallet},..} = do
  let -- Payment info
      PaymentWallet{paymentAddress,paymentKeyPath,stakeAddress,stakeKeyPath} = selectedWallet
      addrInfo = inspectBech32Address $ unPaymentAddress paymentAddress
      spendingKeyHash = fromMaybe "" $ either (const Nothing) infoSpendingKeyHash addrInfo

      -- Stake info
      hasStaking = isJust stakeAddress
      stakeKeyHash = either (const Nothing) infoStakeKeyHash addrInfo
      knownStakeWallets = knownWallets ^. #stakeWallets
      mTrackedStakeWallet = flip (maybe Nothing) stakeAddress $ 
        \addr -> find (\stake -> addr == stake ^. #stakeAddress) knownStakeWallets

  vstack 
    [ centerWidgetH $ label "Address Info" 
        `styleBasic` [textFont "Italics", textSize 18, padding 10]
    , vstack
        [ hstack
            [ spacer_ [width 10]
            , label "Payment Credential" 
                `styleBasic` [textSize 14, textColor lightGray, textFont "Italics"]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelWith "Address:" formatAddress paymentAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer_ [width 20]
            , copyableLabelFor "Key Hash:" $ show $ PubKeyHash $ BuiltinByteString spendingKeyHash
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Derivation Path:" $ maybe "none" display paymentKeyPath
            ] `styleBasic` [padding 5]
              `nodeVisible` isJust paymentKeyPath
        ]
    , spacer
    , separatorLine `styleBasic` [paddingL 20, paddingR 20]
    , spacer
    , vstack
        [ hstack
            [ spacer_ [width 10]
            , label "Stake Credential" 
                `styleBasic` [textSize 14, textColor lightGray, textFont "Italics"]
            , spacer
            , widgetMaybe mTrackedStakeWallet $ \StakeWallet{alias} ->
                label (show $ tupled [pretty alias])
                  `styleBasic`
                    [ padding 0
                    , radius 5
                    , textMiddle
                    , border 0 transparent
                    , textColor lightGray
                    , bgColor transparent
                    , textSize 14
                    ]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Address:" $ maybe "none" toText stakeAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer_ [width 20]
            , copyableLabelFor "Key Hash:" $ 
                maybe "" (show . PubKeyHash . BuiltinByteString) stakeKeyHash
            ] `styleBasic` [padding 5]
              `nodeVisible` hasStaking
        , widgetMaybe stakeKeyPath $ \keyPath -> hstack
            [ spacer_ [width 20]
            , copyableLabelFor "Derivation Path:" $ display keyPath
            ] `styleBasic` [padding 5]
              `nodeVisible` hasStaking
        ]
    ] `styleBasic`
        [ padding 10
        , radius 20
        , bgColor customGray3
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | An address with delegation can be very long and exceed the length of the window. This
-- function will add an ellipsis in the middle if the address is likely to overflow.
formatAddress :: PaymentAddress -> Text
formatAddress (PaymentAddress text)
    | Text.length text > 80 = newText
    | otherwise = text
  where
    newText = Text.take 40 text <> "..." <> Text.drop 80 text

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
          , textSize 12
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label_ info [ellipsis,resizeFactor 2] `styleBasic` [textSize 12, textColor lightGray]
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
          , textSize 12
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label_ formattedInfo [resizeFactor 2] `styleBasic` [textSize 12, textColor lightGray]
    ]
