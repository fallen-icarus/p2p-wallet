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
import P2PWallet.Data.Core.Internal
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder
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
              [ widgetIf ([] /= txBuilderModel ^. #keyWitnesses && txBuilderModel ^. #isBuilt) $ 
                  box_ [alignBottom,alignLeft] $ reqKeysPopup model
              , filler
              , box_ [alignMiddle] $ hstack
                  [ spacer_ [width 50]
                  , mainButton "Build" (TxBuilderEvent $ BuildTx $ StartProcess Nothing)
                      `nodeVisible` isBuildable
                  , button "Build" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` not isBuildable
                  , spacer_ [width 3]
                  , mainButton "Sign & Submit" (TxBuilderEvent $ WitnessTx $ StartProcess Nothing)
                      `nodeVisible` and
                        [ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == PairedTx
                        ]
                  , button "Sign & Submit" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` and
                        [ not $ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == PairedTx
                        ]
                  , mainButton "Witness & Export" 
                        (TxBuilderEvent $ GetTxFileExportDirectory $ StartAdding Nothing)
                      `nodeVisible` and
                        [ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == HybridTx
                        ]
                  , button "Witness & Export" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` and
                        [ not $ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == HybridTx
                        ]
                  , mainButton "Export"
                        (TxBuilderEvent $ GetTxFileExportDirectory $ StartAdding Nothing)
                      `nodeVisible` and
                        [ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == WatchedTx
                        ]
                  , button "Export" AppInit
                      `nodeEnabled` False -- This button is just for show.
                      `nodeVisible` and
                        [ not $ txBuilderModel ^. #isBuilt
                        , txBuilderModel ^. #txType == WatchedTx
                        ]
                  ] `nodeVisible` not isEmpty
              , filler
              , box_ [alignBottom,alignRight] $ toolsPopup model
              ]
          ] `nodeVisible` and
              [ isNothing targetUserOutput
              , isNothing (swapBuilderModel ^. #targetSwapCreation)
              , isNothing (swapBuilderModel ^. #targetSwapUpdate)
              , isNothing (swapBuilderModel ^. #targetSwapExecution)
              , isNothing (loanBuilderModel ^. #targetAskCreation)
              , isNothing (loanBuilderModel ^. #targetAskUpdate)
              , isNothing (loanBuilderModel ^. #targetOfferCreation)
              , isNothing (loanBuilderModel ^. #targetOfferUpdate)
              , isNothing (loanBuilderModel ^. #targetOfferAcceptance)
              , isNothing (loanBuilderModel ^. #targetLoanPayment)
              , isNothing (loanBuilderModel ^. #targetAddressUpdate)
              , isNothing (optionsBuilderModel ^. #targetProposalCreation)
              , isNothing (optionsBuilderModel ^. #targetProposalUpdate)
              , isNothing (optionsBuilderModel ^. #targetAddressUpdate)
              , isNothing (aftermarketBuilderModel ^. #targetSaleCreation)
              , isNothing (aftermarketBuilderModel ^. #targetSaleUpdate)
              , isNothing (aftermarketBuilderModel ^. #targetLoanKeySpotPurchase)
              , isNothing (aftermarketBuilderModel ^. #targetBidCreation)
              , isNothing (aftermarketBuilderModel ^. #targetBidUpdate)
              , isNothing (aftermarketBuilderModel ^. #targetClaimBidAcceptance)
              , isNothing (aftermarketBuilderModel ^. #targetLoanKeyBidClaim)
              , isNothing (aftermarketBuilderModel ^. #targetOptionsKeySpotPurchase)
              , isNothing (aftermarketBuilderModel ^. #targetOptionsKeyBidClaim)
              , not addingChangeOutput
              , not addingExternalUserOutput
              , not addingTestMint
              , not importing
              , not exporting
              ]
      , editUserOutputWidget (maybe "" (view (_2 % #alias)) targetUserOutput)
          `nodeVisible` isJust targetUserOutput
      , editSwapCreationWidget model
          `nodeVisible` isJust (swapBuilderModel ^. #targetSwapCreation)
      , editSwapUpdateWidget model
          `nodeVisible` isJust (swapBuilderModel ^. #targetSwapUpdate)
      , editSwapExecutionWidget model
          `nodeVisible` isJust (swapBuilderModel ^. #targetSwapExecution)
      , editAskCreationWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetAskCreation)
      , editAskUpdateWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetAskUpdate)
      , editOfferCreationWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetOfferCreation)
      , editOfferUpdateWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetOfferUpdate)
      , editOfferAcceptanceWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetOfferAcceptance)
      , editLoanPaymentWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetLoanPayment)
      , editLenderAddressUpdateWidget model
          `nodeVisible` isJust (loanBuilderModel ^. #targetAddressUpdate)
      , editProposalCreationWidget model
          `nodeVisible` isJust (optionsBuilderModel ^. #targetProposalCreation)
      , editProposalUpdateWidget model
          `nodeVisible` isJust (optionsBuilderModel ^. #targetProposalUpdate)
      , editWriterAddressUpdateWidget model
          `nodeVisible` isJust (optionsBuilderModel ^. #targetAddressUpdate)
      , editSaleCreationWidget model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetSaleCreation)
      , editSaleUpdateWidget model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetSaleUpdate)
      , editLoanKeySpotPurchase model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetLoanKeySpotPurchase)
      , editBidCreationWidget model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetBidCreation)
      , editBidUpdateWidget model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetBidUpdate)
      , editClaimBidAcceptance model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetClaimBidAcceptance)
      , editLoanKeyBidClaim model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetLoanKeyBidClaim)
      , editOptionsKeySpotPurchase model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetOptionsKeySpotPurchase)
      , editOptionsKeyBidClaim model
          `nodeVisible` isJust (aftermarketBuilderModel ^. #targetOptionsKeyBidClaim)
      , addExternalUserOutputWidget
          `nodeVisible` addingExternalUserOutput
      , addChangeOutputWidget
          `nodeVisible` addingChangeOutput
      , addTestMintWidget model
          `nodeVisible` addingTestMint
      , importSignedTxWidget
          `nodeVisible` importing
      , exportDestinationWidget
          `nodeVisible` exporting
      ] `styleBasic`
          [ padding 20
          ]
  where
    isEmpty :: Bool
    isEmpty = isEmptyBuilder txBuilderModel

    TxBuilderModel{..} = txBuilderModel

    isBuildable = isRight $ canBeBuilt txBuilderModel

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

actionsList :: AppModel -> AppNode
actionsList AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap,config} = do
  let numActions = length userOutputs
                 + length userCertificates
                 + length userWithdrawals
                 + maybe 0 (const 1) testMint
                 + swapsActionCount swapBuilderModel
                 + loanActionCount loanBuilderModel
                 + optionsActionCount optionsBuilderModel
                 + aftermarketActionCount aftermarketBuilderModel
  vstack
    [ label ("Actions " <> show (tupled [pretty numActions]))
        `styleBasic` [textSize 12]
    , flip styleBasic [padding 10] $ vstack_ [childSpacing_ 5] $ mconcat
        -- Normal outputs
        [ userOutputsList reverseTickerMap userOutputs
        -- Swaps
        , swapCreationsList reverseTickerMap $ swapBuilderModel ^. #swapCreations
        , swapClosesList reverseTickerMap $ swapBuilderModel ^. #swapCloses
        , swapUpdatesList reverseTickerMap $ swapBuilderModel ^. #swapUpdates
        , swapExecutionsList reverseTickerMap $ swapBuilderModel ^. #swapExecutions
        -- Loans
        , askCreationsList reverseTickerMap $ loanBuilderModel ^. #askCreations
        , askClosesList reverseTickerMap $ loanBuilderModel ^. #askCloses
        , askUpdatesList reverseTickerMap $ loanBuilderModel ^. #askUpdates
        , offerCreationsList reverseTickerMap (config ^. #timeZone) $ 
            loanBuilderModel ^. #offerCreations
        , offerClosesList reverseTickerMap (config ^. #timeZone) $ loanBuilderModel ^. #offerCloses
        , offerUpdatesList reverseTickerMap (config ^. #timeZone) $ loanBuilderModel ^. #offerUpdates
        , offerAcceptancesList reverseTickerMap $ loanBuilderModel ^. #offerAcceptances
        , loanPaymentsList reverseTickerMap $ loanBuilderModel ^. #loanPayments
        , interestApplicationsList $ loanBuilderModel ^. #interestApplications
        , expiredClaimsList $ loanBuilderModel ^. #expiredClaims
        , loanKeyBurnsList $ loanBuilderModel ^. #keyBurns
        , lenderAddressUpdatesList $ loanBuilderModel ^. #addressUpdates
        -- Options
        , proposalCreationsList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #proposalCreations
        , proposalClosesList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #proposalCloses
        , proposalUpdatesList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #proposalUpdates
        , proposalPurchasesList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #proposalPurchases
        , expiredOptionsClosesList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #expiredCloses
        , writerAddressUpdatesList $ optionsBuilderModel ^. #addressUpdates
        , optionsKeyBurnsList $ optionsBuilderModel ^. #keyBurns
        , optionsContractExecutionsList reverseTickerMap (config ^. #timeZone) $ 
            optionsBuilderModel ^. #contractExecutions
        -- Aftermarket
        , saleCreationsList reverseTickerMap $ aftermarketBuilderModel ^. #saleCreations
        , saleClosesList reverseTickerMap $ aftermarketBuilderModel ^. #saleCloses
        , saleUpdatesList reverseTickerMap $ aftermarketBuilderModel ^. #saleUpdates
        , loanKeySpotPurchasesList reverseTickerMap $ aftermarketBuilderModel ^. #loanKeySpotPurchases
        , bidCreationsList reverseTickerMap $ aftermarketBuilderModel ^. #bidCreations
        , bidClosesList reverseTickerMap $ aftermarketBuilderModel ^. #bidCloses
        , bidUpdatesList reverseTickerMap $ aftermarketBuilderModel ^. #bidUpdates
        , claimBidAcceptancesList reverseTickerMap $ aftermarketBuilderModel ^. #claimBidAcceptances
        , loanKeyBidClaimsList reverseTickerMap $ aftermarketBuilderModel ^. #loanKeyBidClaims
        , optionsKeySpotPurchasesList reverseTickerMap $ aftermarketBuilderModel ^. #optionsKeySpotPurchases
        , spotBidAcceptancesList reverseTickerMap $ aftermarketBuilderModel ^. #spotBidAcceptances
        , optionsKeyBidClaimsList reverseTickerMap $ aftermarketBuilderModel ^. #optionsKeyBidClaims
        , bidUnlocksList $ aftermarketBuilderModel ^. #bidUnlocks
        -- Certificates
        , userCertificatesList userCertificates
        -- Withdrawals
        , userWithdrawalsList userWithdrawals
        -- Test mints
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

addExternalUserOutputWidget :: AppNode
addExternalUserOutputWidget = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Recipient:"
        , spacer
        , textField_ (toLensVL $ #txBuilderModel % #newExternalUserOutput % #alias)
              [placeholder "Bob"]
            `styleBasic` [textSize 16, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack 
        [ label "Payment Address:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #newExternalUserOutput % #paymentAddress)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ label "ADA:"
        , spacer
        , textField_ (toLensVL $ #txBuilderModel % #newExternalUserOutput % #ada)
              [placeholder "1.234567"]
            `styleBasic` [width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ label "Native Assets (separated with newlines):"
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
    , textArea (toLensVL $ #txBuilderModel % #newExternalUserOutput % #nativeAssets)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ AddNewExternalUserOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ AddNewExternalUserOutput ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

importSignedTxWidget :: AppNode
importSignedTxWidget = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Absolute FilePath:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #targetPath)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , mainButton helpIcon (Alert filePathMsg)
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
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ ImportSignedTxFile CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ ImportSignedTxFile ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

exportDestinationWidget :: AppNode
exportDestinationWidget = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Absolute Directory Path:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #targetPath)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , mainButton helpIcon (Alert directoryPathMsg)
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
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ GetTxFileExportDirectory CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ GetTxFileExportDirectory ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

toolsPopup :: AppModel -> AppNode
toolsPopup AppModel{..} = do
  let anchor = 
        tooltip_ "Tools" [tooltipDelay 0] $
          button toolsIcon (TxBuilderEvent ShowTxToolsPopup)
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
    [ customPopup_ (toLensVL $ #txBuilderModel % #showToolsPopup) 
        [popupAnchor anchor, alignTop, alignLeft, popupAlignToOuterV] $
        vstack
          [ button "External Output" 
                (TxBuilderEvent $ AddNewExternalUserOutput $ StartAdding Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , button "Change Output" 
                (TxBuilderEvent $ AddNewChangeOutput $ StartAdding Nothing)
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

reqKeysPopup :: AppModel -> AppNode
reqKeysPopup AppModel{..} = do
  let anchor = 
        tooltip_ "Required Signatures" [tooltipDelay 0] $
          button reqKeysIcon (TxBuilderEvent ShowTxKeysPopup)
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
      (aliases, pkhs, derivs) = unzip3 $ map unKeyWitness $ txBuilderModel ^. #keyWitnesses
      row txt = box_ [alignMiddle, alignCenter] $ label txt `styleBasic` [textSize 9]
      header txt = [ row txt
                   , separatorLine `styleBasic` [fgColor lightGray]
                   ]
  vstack
    [ customPopup_ (toLensVL $ #txBuilderModel % #showKeysPopup) 
        [popupAnchor anchor, alignTop, alignRight, popupAlignToOuterV] $
        flip styleBasic [bgColor customGray3, border 1 black, padding 5] $
          hstack_ [childSpacing_ 7]
            [ vstack_ [childSpacing_ 3] $ header "Alias" <> map row aliases 
            , separatorLine `styleBasic` [fgColor lightGray]
            , vstack_ [childSpacing_ 3] $ header "Hash" <> map (row . show) pkhs 
            , separatorLine `styleBasic` [fgColor lightGray]
            , vstack_ [childSpacing_ 3] $ header "Derivation" <> for derivs (\der -> case der of
                Just d -> row $ display d
                Nothing -> row "Watched Key")
            ]
    ] `styleBasic` [padding 0]
