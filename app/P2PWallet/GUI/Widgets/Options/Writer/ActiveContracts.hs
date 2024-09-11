module P2PWallet.GUI.Widgets.Options.Writer.ActiveContracts
  ( activeContractsWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map
import Data.List (minimum, maximum)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

activeContractsWidget :: AppModel -> AppNode
activeContractsWidget model@AppModel{knownWallets,optionsModel=OptionsModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , updateAddressWidget `nodeVisible` isJust (writerModel ^. #newWriterAddressUpdate)
      -- , activeFilterWidget model `nodeVisible` (writerModel ^. #showActiveFilter)
      ]
  where
    Config{timeZone,currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    OptionsWallet{..} = selectedWallet

    allActives :: [OptionsUTxO]
    allActives = filter ((==Just True) . fmap (is _OptionsActiveDatum) . view #optionsDatum) utxos

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allActives)

    sample :: [OptionsUTxO]
    sample = allActives
    
    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ centerWidgetH $ hstack 
            [ box_ [alignMiddle, onClick $ Alert activeOptionsMsg] $
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
            , spacer_ [width 5]
            , label ("Active Contracts (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #optionsModel % #writerModel % #showActiveFilter)
                  [toggleButtonOffStyle toggleOffStyle]
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , paddingT 0
                    , paddingB 0
                    , paddingL 5
                    , paddingR 5
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ]
        , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
            vstack_ [childSpacing] (map activeRow sample)
              `styleBasic` [padding 10]
        , filler
        ] 

    activeRow :: OptionsUTxO -> AppNode
    activeRow u@OptionsUTxO{utxoRef,blockTime} = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
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
          (actionEvt, actionTip, actionIcon)
            | toPlutusTime (config ^. #currentTime) >= expiration =
                ( OptionsEvent $ OptionsWriterEvent $ AddSelectedExpiredOptionsClose u
                , "Close expired options contract"
                , closeCircleIcon
                )
            | otherwise =
                ( OptionsEvent $ OptionsWriterEvent $ 
                    UpdateWriterPaymentAddress $ StartAdding $ Just u
                , "Update payment address"
                , updatePaymentAddressIcon
                )
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
                , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                    label clockIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
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
                        label idCardIcon
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ actionTip [tooltipDelay 0] $
                button actionIcon actionEvt 
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
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

updateAddressWidget :: AppNode
updateAddressWidget = do
  let maybeLens' = maybeLens def (#optionsModel % #writerModel % #newWriterAddressUpdate)
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label "Where would you like the payment to go?"
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
            , button "Cancel" $ OptionsEvent $ OptionsWriterEvent $ 
                UpdateWriterPaymentAddress CancelAdding
            , spacer
            , mainButton "Confirm" $ OptionsEvent $ OptionsWriterEvent $ 
                UpdateWriterPaymentAddress ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

