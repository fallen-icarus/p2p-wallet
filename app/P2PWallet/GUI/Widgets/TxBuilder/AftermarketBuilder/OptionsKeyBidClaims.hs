module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.OptionsKeyBidClaims
  ( 
    optionsKeyBidClaimsList
  , editOptionsKeyBidClaim
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

optionsKeyBidClaimsList :: ReverseTickerMap -> [(Int,OptionsKeyAcceptedBidClaim)] -> [AppNode]
optionsKeyBidClaimsList reverseTickerMap = map utxoRow
  where
    priceWidget :: NativeAsset -> AppNode
    priceWidget priceAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap priceAsset)
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

    utxoRow :: (Int,OptionsKeyAcceptedBidClaim) -> AppNode
    utxoRow s@(idx,OptionsKeyAcceptedBidClaim{..}) = do
      let prices = maybe [] (map toNativeAsset . Aftermarket.unPrices)
                 $ aftermarketUTxOBuyerPrice 
                 $ bidClaim ^. #bidUTxO
          (_, nfts) = fromMaybe ("",[]) $ aftermarketUTxONfts $ bidClaim ^. #bidUTxO
          numberSold = length nfts
      hstack
        [ vstack
            [ hstack
                [ label "Claim Accepted Bid"
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (show numberSold <> " Options Key(s)")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label "Bid:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ spacer : map priceWidget p
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignMiddle] $ hstack
            [ box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (aftermarketBuilderEvent $ EditSelectedOptionsKeyBidClaim $ StartAdding $ Just s)
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
            , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedOptionsKeyBidClaim idx)
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

editOptionsKeyBidClaim :: AppModel -> AppNode
editOptionsKeyBidClaim model = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetOptionsKeyBidClaim)
      contracts = model ^. maybeLens' % _2 % #contracts
  centerWidget $ vstack
    [ centerWidgetH $ label "Which contracts would you like to immediately execute?"
    , spacer_ [width 20]
    , vscroll_ [wheelRate 50] $
        vstack_ [childSpacing] $ 
          map (executeOptionsWidget model maybeLens') $ zip [0..] contracts
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ aftermarketBuilderEvent $ EditSelectedOptionsKeyBidClaim CancelAdding
        , spacer
        , mainButton "Confirm" $ aftermarketBuilderEvent $ EditSelectedOptionsKeyBidClaim ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

executeOptionsWidget 
  :: AppModel
  -> Lens' AppModel (Int,NewOptionsKeyAcceptedBidClaim)
  -> (Int,(Bool,OptionsUTxO)) 
  -> AppNode
executeOptionsWidget AppModel{..} maybeLens' (idx,(_,u@OptionsUTxO{..})) = do
  let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
      offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
      askNativeAsset = toNativeAsset askAsset
      payToAddress = either (const "error") fst $ plutusToBech32 (config ^. #network) paymentAddress
      mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                    $ knownWallets ^. #paymentWallets
      addressTip = unwords $ filter (/= "")
        [ "Payments to"
        , maybe ":" ((<> ":") . view #alias) mTargetWallet
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
  hstack
    [ vstack
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
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Options Contract ID: " <> display contractId) [tooltipDelay 0] $
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
    , spacer
    , checkbox_ (toLensVL $ maybeLens' % _2 % #contracts % toggleExecution idx) [checkboxSquare]
        `styleBasic` [fgColor customGray1, hlColor customBlue]
    ]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `Bool` field of the NewOptionsKeyAcceptedBidClaim contracts list.
toggleExecution :: Int -> Lens' [(Bool,OptionsUTxO)] Bool
toggleExecution idx = lens getToggleExecution setToggleExecution
  where
    getToggleExecution :: [(Bool,OptionsUTxO)] -> Bool
    getToggleExecution us = fromMaybe False $ us ^? ix idx % _1

    setToggleExecution :: [(Bool,OptionsUTxO)] -> Bool -> [(Bool,OptionsUTxO)]
    setToggleExecution us b = us & ix idx % _1 .~ b
