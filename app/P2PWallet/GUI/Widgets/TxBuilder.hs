module P2PWallet.GUI.Widgets.TxBuilder
  ( 
    txBuilderWidget
  ) where

import Monomer hiding 
  ( popupAnchor
  , alignTop
  , popupAlignToOuterV
  , popupAlignToOuterH
  )
import Prettyprinter (pretty, align, vsep, tupled)
import Data.Text qualified as Text
import Data.Map qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Core.TxBody
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Plutus
import P2PWallet.Prelude

txBuilderWidget :: AppModel -> AppNode
txBuilderWidget model@AppModel{..} = do
    zstack
      [ vstack
          [ zstack
              [ widgetIf (not isEmpty) mainWidget
              , widgetIf isEmpty $ vstack
                  [ centerWidget $
                      flip styleBasic [bgColor transparent, padding 20, radius 5] $ box $ 
                        label "Add something to the builder to begin!"
                         `styleBasic` [textFont "Italics"]
                  , filler
                  ]
              ]
          , filler
          , hstack
              [ filler
              , box_ [alignMiddle] $ hstack
                  [ spacer_ [width 50]
                  , mainButton "Build" (TxBuilderEvent $ BuildTx StartProcess)
                      `nodeVisible` canBeBuilt
                  , button "Build" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` not canBeBuilt
                  , spacer_ [width 3]
                  , mainButton "Sign & Submit" (TxBuilderEvent $ WitnessTx StartProcess)
                      `nodeVisible` txBuilderModel ^. #isBuilt
                  , button "Sign & Submit" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` not (txBuilderModel ^. #isBuilt)
                  ] `nodeVisible` not isEmpty
              , filler
              , box_ [alignBottom,alignRight] addPopup
              ]
          ] `nodeVisible` and
              [ isNothing targetUserOutput
              , not isAddingChangeOutput
              , not isAddingTestMint
              , not isImporting
              ]
      , editUserOutputWidget (maybe "" (view (_2 % #alias)) targetUserOutput)
          `nodeVisible` isJust targetUserOutput
      , addChangeOutputWidget
          `nodeVisible` isAddingChangeOutput
      , addTestMintWidget model
          `nodeVisible` isAddingTestMint
      , importSignedTxWidget
          `nodeVisible` isImporting
      ] `styleBasic`
          [ padding 20
          ]
  where
    isEmpty :: Bool
    isEmpty = isEmptyBuilder txBuilderModel

    canBeBuilt :: Bool
    canBeBuilt = and
      -- A change output must be set with a valid payment address.
      [ maybe False (("" /=) . view #paymentAddress) $ txBuilderModel ^. #changeOutput
      -- The transaction must be balanced.
      , txBuilderModel ^. #isBalanced
      -- If it requires collateral, then a colalteral input must be set.
      , bool True (isJust $ txBuilderModel ^. #collateralInput) $ 
          txBuilderModel ^. #requiresCollateral
      -- At least one input must be specified.
      , txBuilderModel ^. #userInputs /= []
      ]

    isAddingChangeOutput :: Bool
    isAddingChangeOutput = txBuilderModel ^. #addingChangeOutput

    isAddingTestMint :: Bool
    isAddingTestMint = txBuilderModel ^. #addingTestMint

    isImporting :: Bool
    isImporting = txBuilderModel ^. #importing

    targetUserOutput :: Maybe (Int,NewUserOutput)
    targetUserOutput = txBuilderModel ^. #targetUserOutput

    mainWidget :: AppNode
    mainWidget = do
      vstack
        [ centerWidgetH $ label "Tx Builder"
            `styleBasic` 
              [ paddingT 10
              , paddingB 10
              , textFont "Italics"
              , textColor white
              , textSize 18
              ]
        , box_ [alignMiddle] $ hstack
            [ changeInfoPopup model
            , spacer_ [width 2]
            , statusBar model
            , spacer_ [width 2]
            , collateralInfoPopup model `nodeVisible` isJust (txBuilderModel ^. #collateralInput)
            ]
        , vscroll_ [wheelRate 50] $
            vstack 
              [ actionsList model
              , widgetIf (txBuilderModel ^. #userInputs /= []) $ userInputsList model
              ]
        ]

    addPopup :: AppNode
    addPopup = do
      let anchor = 
            button commandIcon (TxBuilderEvent ShowTxAddPopup)
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , paddingT 2
                , paddingB 2
                , bgColor black
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
      vstack
        [ customPopup_ (toLensVL $ #txBuilderModel % #showAddPopup) 
            [popupAnchor anchor, alignTop, alignLeft, popupAlignToOuterV] $
            vstack
              [ button "Change Output" (TxBuilderEvent $ AddNewChangeOutput $ StartAdding Nothing)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              -- Minting test tokens is only available for the testnet.
              , widgetIf (config ^. #network == Testnet) $
                  button "Mint Test Tokens" (TxBuilderEvent $ AddNewTestMint $ StartAdding Nothing)
                    `styleBasic`
                      [ border 0 transparent
                      , textSize 12
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , separatorLine `styleBasic` [fgColor black, padding 5]
              , button "Submit External Tx" (TxBuilderEvent $ ImportSignedTxFile $ StartAdding Nothing)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , button "Reset Builder" (TxBuilderEvent ResetBuilder)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customRed
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ] `styleBasic`
                  [ bgColor customGray3
                  , border 1 black
                  , padding 5
                  ]
        ] `styleBasic` [padding 0]

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
      hasInputs = userInputs /= []
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
                , flip styleBasic [textSize 8, maxWidth 300] $
                    copyableTextArea $ show $ align $ vsep $ 
                      map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
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

userInputsList :: AppModel -> AppNode
userInputsList AppModel{txBuilderModel=TxBuilderModel{userInputs},reverseTickerMap} = do
  vstack
    [ label ("Personal UTxOs " <> show (tupled [pretty $ length userInputs]))
        `styleBasic` [textSize 12]
    , vstack_ [childSpacing] (map utxoRow userInputs)
        `styleBasic` [padding 10]
    ] `styleBasic` [padding 5]
  where
    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    specificUtxoMoreOffStyle :: Style
    specificUtxoMoreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoRow :: (Int,UserInput) -> AppNode
    utxoRow (idx,u@UserInput{..}) =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf (display utxoRef) white 10
                , filler
                , label (display lovelace) 
                    `styleBasic` [textSize 10, textColor white]
                ]
            , hstack
                [ label ("From: " <> walletAlias)
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , widgetIf (not $ null nativeAssets) $ 
                    tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , tooltip_ "Remove UTxO" [tooltipDelay 0] $
                    button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserInput idx)
                      `styleBasic` 
                        [ textSize 10
                        , textColor customRed
                        , textFont "Remix"
                        , textMiddle
                        , padding 0
                        , bgColor transparent
                        , border 0 transparent
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                , spacer_ [width 5]
                , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                    toggleButton_ horizontalMoreIcon
                      (toLensVL $ #txBuilderModel 
                                % #userInputs 
                                % toggleUserInputDetails idx)
                      [toggleButtonOffStyle specificUtxoMoreOffStyle]
                      `styleBasic` 
                        [ textSize 10
                        , textColor customRed
                        , textFont "Remix"
                        , textMiddle
                        , padding 0
                        , bgColor transparent
                        , border 0 transparent
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
               ] 
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , widgetIf showDetails $ utxoDetails u
        ]

    utxoDetails :: UserInput -> AppNode
    utxoDetails UserInput{..} = 
      hstack
        [ filler
        , vstack
            [ copyableLabelFor 8 lightGray "Payment Address:" (toText paymentAddress)
                `styleBasic` [padding 2]
            , widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 8, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , flip styleBasic [textSize 8, textColor lightGray, maxWidth 300] $
                          copyableTextArea $ show $ align $ vsep $ 
                            map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]

actionsList :: AppModel -> AppNode
actionsList AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let numActions = length userOutputs
                 + length userCertificates
                 + length userWithdrawals
                 + maybe 0 (const 1) testMint
  vstack
    [ label ("Actions " <> show (tupled [pretty numActions]))
        `styleBasic` [textSize 12]
    , flip styleBasic [padding 10] $ vstack_ [childSpacing_ 5] $ mconcat
        [ userOutputsList reverseTickerMap userOutputs
        , maybe [] (pure . testMintRow reverseTickerMap) testMint
        , userCertificatesList userCertificates
        , userWithdrawalsList userWithdrawals
        ]
    ] `styleBasic` [padding 5]

userCertificatesList :: [(Int,UserCertificate)] -> [AppNode]
userCertificatesList userCertificates = map certificateRow userCertificates
  where
    certificateRow :: (Int,UserCertificate) -> AppNode
    certificateRow (idx,UserCertificate{..}) = do
      let mainLabelCaption = fromString $ case certificateAction of
            Registration -> printf "Register %s" walletAlias
            Deregistration -> printf "Deregister %s" walletAlias
            Delegation _ -> printf "Delegate %s" walletAlias
      hstack
        [ vstack
            [ hstack
                [ label mainLabelCaption
                    `styleBasic` [textSize 10, textColor white]
                , filler
                , widgetMaybe poolName $ \name ->
                    label name
                      `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ copyableLabelSelf (toText stakeAddress) lightGray 8
                , filler
                , widgetMaybe (certificateAction ^? _Delegation) $ \poolId ->
                    copyableLabelSelfWith 8 trimBech32 poolId lightGray
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserCertificate idx)
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

userWithdrawalsList :: [(Int,UserWithdrawal)] -> [AppNode]
userWithdrawalsList userWithdrawals = map withdrawalRow userWithdrawals
  where
    withdrawalRow :: (Int,UserWithdrawal) -> AppNode
    withdrawalRow (idx,UserWithdrawal{..}) = do
      let mainLabelCaption = fromString $
            printf "Withdraw Rewards from %s" walletAlias
      hstack
        [ vstack
            [ hstack
                [ label mainLabelCaption
                    `styleBasic` [textSize 10, textColor white]
                , filler
                , label (display lovelace)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ copyableLabelSelf (toText stakeAddress) lightGray 8
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserWithdrawal idx)
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

testMintRow :: ReverseTickerMap -> TestMint -> AppNode
testMintRow reverseTickerMap TestMint{..} = do
    hstack
      [ vstack
          [ hstack
              [ label "Mint/Burn Test Tokens" `styleBasic` [textSize 10]
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

userOutputsList :: ReverseTickerMap -> [(Int,UserOutput)] -> [AppNode]
userOutputsList reverseTickerMap userOutputs = map utxoRow userOutputs
  where
    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    specificUtxoMoreOffStyle :: Style
    specificUtxoMoreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoRow :: (Int,UserOutput) -> AppNode
    utxoRow o@(idx,u@UserOutput{..}) =
      hstack
        [ vstack
            [ vstack
                [ hstack 
                    [ label ("Pay " <> if alias == "" then "external address" else alias)
                        `styleBasic` [textSize 10, textColor white]
                    , spacer_ [width 5]
                    , widgetIf (not $ null nativeAssets) $ 
                        tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                          `styleBasic` 
                            [ textSize 10
                            , textColor customBlue
                            , textFont "Remix"
                            , textMiddle
                            ]
                    , filler
                    , label (display lovelace) 
                        `styleBasic` [textSize 10, textColor white]
                    ]
                , spacer_ [width 2]
                , hstack
                    [ copyableLabelSelf (toText paymentAddress) lightGray 8
                    , filler
                    , widgetIf (nativeAssets /= []) $ hstack
                        [ spacer_ [width 5]
                        , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                            toggleButton_ horizontalMoreIcon
                              (toLensVL $ #txBuilderModel 
                                        % #userOutputs 
                                        % toggleUserOutputDetails idx)
                              [toggleButtonOffStyle specificUtxoMoreOffStyle]
                              `styleBasic` 
                                [ textSize 10
                                , textColor customRed
                                , textFont "Remix"
                                , textMiddle
                                , padding 0
                                , bgColor transparent
                                , border 0 transparent
                                ]
                              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                        ]
                   ] 
                ] `styleBasic` 
                    [ padding 10
                    , bgColor customGray2
                    , radius 5
                    , border 1 black
                    ]
            , widgetIf showDetails $ utxoDetails u
            ]
        , spacer_ [width 3]
        , hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (TxBuilderEvent $ EditSelectedUserOutput $ StartAdding $ Just o)
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
                ]
            , spacer_ [width 2]
            , countWidget idx count
            , spacer_ [width 2]
            , vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserOutput idx)
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
        ]

    countWidget :: Int -> Int -> AppNode
    countWidget idx count = do
      let upperCount = count + 1
          lowerCount = count - 1
      vstack
        [ box_ [onClick $ TxBuilderEvent $ ChangeUserOutputCount idx upperCount] $
            label ascendingSortIcon
              `styleBasic`
                [ textSize 14
                , textColor lightGray
                , padding 0
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover`
                [ textColor customBlue
                , cursorIcon CursorHand
                ]
        , flip styleBasic [padding 3, bgColor customGray2] $ 
            box $ label (show count) `styleBasic` [textSize 12, padding 0, textColor customBlue]
        , flip nodeEnabled (lowerCount > 0) $
            box_ [onClick $ TxBuilderEvent $ ChangeUserOutputCount idx lowerCount] $
              label descendingSortIcon
                `styleBasic`
                  [ textSize 14
                  , textColor $ if lowerCount > 0 then lightGray else customGray1
                  , padding 0
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover`
                  [ textColor customBlue
                  , cursorIcon CursorHand
                  ]
        ]

    utxoDetails :: UserOutput -> AppNode
    utxoDetails UserOutput{nativeAssets} = 
      hstack
        [ filler
        , vstack
            [ widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 8, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , flip styleBasic [textSize 8,textColor lightGray, maxWidth 300] $ 
                          copyableTextArea $ show $ align $ vsep $ 
                            map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]

editUserOutputWidget :: Text -> AppNode
editUserOutputWidget recipient = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #targetUserOutput)
  centerWidget $ vstack
    [ centerWidgetH $ label ("How much would you like to send to " <> recipient <> "?")
    , spacer_ [width 20]
    , hstack
        [ label "ADA:"
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #ada)
              [placeholder "1.234567"]
            `styleBasic` [width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ label "Native Assets (separated with newlines)"
        , mainButton helpIcon (Alert nativeAssetAreaEntryMsg)
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
    , textArea (toLensVL $ maybeLens' % _2 % #nativeAssets)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ EditSelectedUserOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ EditSelectedUserOutput ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

addChangeOutputWidget :: AppNode
addChangeOutputWidget = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Change Address:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #newChangeOutput % #paymentAddress)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ AddNewChangeOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ AddNewChangeOutput ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

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
        , box_ [alignMiddle, onClick $ TxBuilderEvent $ ConvertExampleTestMintNameToHexidecimal] $
            label remixArrowRightLine 
              `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
              `styleHover` [bgColor customGray1]
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
        [ label "Token Quantities (separated with newlines)"
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
    ] `styleBasic` [bgColor customGray3, padding 20]

importSignedTxWidget :: AppNode
importSignedTxWidget = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Absolute FilePath:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #importedSignedTxFile)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ ImportSignedTxFile CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ ImportSignedTxFile ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Trim the stake address or pool id so that they fit better on the same line.
trimBech32 :: (ToText a) => a -> Text
trimBech32 info = Text.take 25 text <> "..." <> Text.drop 35 text
  where
    text = toText info

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

-- | A label button that will copy other data. The font size is configurable.
copyableLabelFor :: Double -> Color -> Text -> Text -> WidgetNode s AppEvent
copyableLabelFor fontSize color caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText info)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          , textSize fontSize
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label_ info [ellipsis] `styleBasic` [textColor color, textSize fontSize]
    ]

copyableLabelSelfWith :: (ToText a) => Double -> (a -> Text) -> a -> Color -> WidgetNode s AppEvent
copyableLabelSelfWith fontSize modifier fullInfo color = do
  let formattedInfo = modifier fullInfo
  tooltip_ "Copy" [tooltipDelay 0] $ button formattedInfo (CopyText $ toText fullInfo)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , border 0 transparent
      , textColor color
      , bgColor transparent
      , textSize fontSize
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
    
-------------------------------------------------
-- Helper Lens
-------------------------------------------------
toggleUserInputDetails :: Int -> Lens' [(Int,UserInput)] Bool
toggleUserInputDetails idx = lens getToggleDetails setToggleDetails
  where
    getToggleDetails :: [(Int,UserInput)] -> Bool
    getToggleDetails [] = False
    getToggleDetails ((i,u):us) =
      if i == idx
      then u ^. #showDetails
      else getToggleDetails us

    setToggleDetails :: [(Int,UserInput)] -> Bool -> [(Int,UserInput)]
    setToggleDetails [] _ = []
    setToggleDetails ((i,u):us) b =
      if i == idx
      then (i,u & #showDetails .~ b) : us
      else (i,u) : setToggleDetails us b

toggleUserOutputDetails :: Int -> Lens' [(Int,UserOutput)] Bool
toggleUserOutputDetails idx = lens getToggleDetails setToggleDetails
  where
    getToggleDetails :: [(Int,UserOutput)] -> Bool
    getToggleDetails [] = False
    getToggleDetails ((i,u):us) =
      if i == idx
      then u ^. #showDetails
      else getToggleDetails us

    setToggleDetails :: [(Int,UserOutput)] -> Bool -> [(Int,UserOutput)]
    setToggleDetails [] _ = []
    setToggleDetails ((i,u):us) b =
      if i == idx
      then (i,u & #showDetails .~ b) : us
      else (i,u) : setToggleDetails us b
