{-# LANGUAGE DataKinds #-}

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
import Prettyprinter (pretty, tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.GUI.Widgets.TxBuilder.StatusBar
import P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder
import P2PWallet.GUI.Widgets.TxBuilder.TestMint
import P2PWallet.GUI.Widgets.TxBuilder.UserCertificates
import P2PWallet.GUI.Widgets.TxBuilder.UserInputs
import P2PWallet.GUI.Widgets.TxBuilder.UserOutputs
import P2PWallet.GUI.Widgets.TxBuilder.UserWithdrawals
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
              , isNothing (swapBuilderModel ^. #targetSwapCreation)
              , isNothing (swapBuilderModel ^. #targetSwapUpdate)
              , not addingChangeOutput
              , not addingTestMint
              , not importing
              ]
      , editUserOutputWidget (maybe "" (view (_2 % #alias)) targetUserOutput)
          `nodeVisible` isJust targetUserOutput
      , editSwapCreationWidget model
          `nodeVisible` isJust (swapBuilderModel ^. #targetSwapCreation)
      , editSwapUpdateWidget model
          `nodeVisible` isJust (swapBuilderModel ^. #targetSwapUpdate)
      , addChangeOutputWidget
          `nodeVisible` addingChangeOutput
      , addTestMintWidget model
          `nodeVisible` addingTestMint
      , importSignedTxWidget
          `nodeVisible` importing
      ] `styleBasic`
          [ padding 20
          ]
  where
    isEmpty :: Bool
    isEmpty = isEmptyBuilder txBuilderModel

    TxBuilderModel{..} = txBuilderModel

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
      , or [ txBuilderModel ^. #userInputs /= []
           , txBuilderModel ^. #swapBuilderModel % #swapCloses /= []
           , txBuilderModel ^. #swapBuilderModel % #swapUpdates /= []
           ]
      ]

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
            tooltip_ "Tools" [tooltipDelay 0] $
              button toolsIcon (TxBuilderEvent ShowTxAddPopup)
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , paddingT 2
                  , paddingB 2
                  , bgColor customGray3
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

actionsList :: AppModel -> AppNode
actionsList AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let numActions = length userOutputs
                 + length userCertificates
                 + length userWithdrawals
                 + maybe 0 (const 1) testMint
                 + swapsActionCount swapBuilderModel
  vstack
    [ label ("Actions " <> show (tupled [pretty numActions]))
        `styleBasic` [textSize 12]
    , flip styleBasic [padding 10] $ vstack_ [childSpacing_ 5] $ mconcat
        [ userOutputsList reverseTickerMap userOutputs
        , swapCreationsList reverseTickerMap $ swapBuilderModel ^. #swapCreations
        , swapClosesList reverseTickerMap $ swapBuilderModel ^. #swapCloses
        , swapUpdatesList reverseTickerMap $ swapBuilderModel ^. #swapUpdates
        , userCertificatesList userCertificates
        , userWithdrawalsList userWithdrawals
        , maybe [] (pure . testMintRow reverseTickerMap) testMint
        ]
    ] `styleBasic` [padding 5]

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

