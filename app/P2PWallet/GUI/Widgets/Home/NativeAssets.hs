module P2PWallet.GUI.Widgets.Home.NativeAssets
  ( 
    nativeAssetsWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Home.NativeAssets.LoanKeys
import P2PWallet.GUI.Widgets.Home.NativeAssets.NftBatch
import P2PWallet.GUI.Widgets.Home.NativeAssets.OptionsKeys
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests qualified as View
import P2PWallet.Prelude

nativeAssetsWidget :: AppModel -> AppNode
nativeAssetsWidget model@AppModel{reverseTickerMap,..} =
    zstack
      [ vstack 
          [ widgetIf (not $ null allAssets) $ vstack
              [ -- A header with a filter button and search bar.
                centerWidgetH $ hstack
                  [ -- A buttom to open the filter settings.
                    tooltip_ "Sort/Filter/Search" [tooltipDelay 0] $
                      toggleButton_ menuSearchIcon
                        (toLensVL $ #homeModel % #showAssetFilter)
                        [toggleButtonOffStyle menuOffStyle]
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
                  , spacer_ [width 5]
                    -- A search bar.
                  , textField_ 
                        (toLensVL $ #homeModel % #assetFilterModel % #search) 
                        [placeholder "one of: full name, policy id, asset name, fingerprint, ticker"] 
                      `styleBasic`
                        [ textSize 12
                        , width 400
                        , bgColor customGray1
                        , sndColor darkGray
                        ]
                      `styleFocus` [border 1 customBlue]
                  , spacer_ [width 5]
                    -- The confirm search button. The redrawing is delayed until this button is
                    -- pressed.
                  , toggleButton_ "Search" (toLensVL #forceRedraw)
                      [toggleButtonOffStyle searchOffStyle]
                      `styleBasic`
                        [ bgColor customBlue
                        , textColor white
                        , textSize 12
                        , border 0 transparent
                        , textSize 10
                        ]
                      `styleHover`
                        [ bgColor lightGray ]
                  ]
                -- The native assets that match those filters and search criteria.
              , widgetIf (not $ null sample) $ cushionWidget $ vscroll_ [wheelRate 50] $ 
                  vstack_ [childSpacing] (map assetRow sample)
                    `styleBasic` 
                      [ padding 10
                      , paddingT 0
                      ]
                -- What to display when no assets match the filter and search criteria.
              , widgetIf (null sample) $ 
                  centerWidget $
                    label "No assets match that search."
                     `styleBasic` [textFont "Italics"]
              ]
            -- What to display when the wallet does not have any native assets.
          , widgetIf (null allAssets) $
              centerWidget $
                label "This address does not have any native assets."
                 `styleBasic` [textFont "Italics"]
          , flip nodeVisible (homeModel ^. #queuedNFTs /= []) $ centerWidgetH $
              hstack
                [ button "Cancel" (HomeEvent $ NftBatchEvent ClearNftBatch) 
                    `styleBasic` [textMiddle, textSize 12]
                , spacer
                , label (show nftCount <> " Key NFT(s)")
                    `styleBasic` [radius 5, textSize 10, textMiddle, padding 5, border 1 customBlue]
                , spacer
                , mainButton "View Batch" (HomeEvent $ NftBatchEvent ViewNftBatch)
                    `styleBasic` [textMiddle, textSize 12]
                ]
          , spacer
          ]
      , assetFilterWidget model `nodeVisible` (homeModel ^. #showAssetFilter)
      , inspectLoanWidget model
          `nodeVisible` and
            [ isJust $ homeModel ^. #inspectedLoan
            -- Hide until after syncing is complete.
            , not $ model ^. #waitingStatus % #syncingLoanHistories
            ]
      , widgetMaybe (homeModel ^. #inspectedBorrower) $ \info ->
          let closeEvt = HomeEvent CloseInspectedKeyBorrowerInformation
              historyEvt = HomeEvent . InspectBorrowerLoan
           in View.inspectBorrowerWidget info closeEvt historyEvt model
                `nodeVisible` and
                  [ -- Hide until after syncing is complete.
                    not $ model ^. #waitingStatus % #syncingBorrowerInfo
                  ]
      , widgetMaybe (homeModel ^. #inspectedBorrowerLoan) $ \targetId ->
          let closeEvt = HomeEvent CloseInspectedBorrowerLoan in
          View.inspectLoanWidget targetId closeEvt model
            `nodeVisible` and
              [ -- Hide until after syncing is complete.
                not $ model ^. #waitingStatus % #syncingLoanHistories
              ]
      , inspectOptionsContractWidget model
          `nodeVisible` and
            [ isJust $ homeModel ^. #inspectedOptionsContract
            -- Hide until after syncing is complete.
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      , viewBatchWidget model
          `nodeVisible` and
            [ homeModel ^. #viewingQueue
            , isNothing $ homeModel ^. #newSaleCreation
            ]
      , createSaleWidget model
          `nodeVisible` isJust (homeModel ^. #newSaleCreation)
      ]
  where
    nftCount = length $ homeModel ^. #queuedNFTs
      
    wallet :: PaymentWallet
    wallet = homeModel ^. #selectedWallet

    allAssets :: [NativeAsset]
    allAssets = wallet ^. #nativeAssets

    menuOffStyle :: Style
    menuOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            , textSize 12
            ]
          `styleHover`
            [ bgColor customGray1]

    searchOffStyle :: Style
    searchOffStyle = 
      def `styleBasic` 
            [ bgColor customBlue
            , textColor white
            , border 0 transparent
            ]
          `styleHover`
            [ bgColor lightGray ]

    AssetFilterModel{search=searchTarget,keyNftType} = homeModel ^. #assetFilterModel

    keyNftFilter :: [NativeAsset] -> [NativeAsset]
    keyNftFilter xs = case keyNftType of
      Just LoanKey -> filter ((Loans.activeBeaconCurrencySymbol==) . view #policyId) xs
      Just OptionsKey -> filter ((Options.activeBeaconCurrencySymbol==) . view #policyId) xs
      _ -> xs

    searchFilter :: [NativeAsset] -> [NativeAsset]
    searchFilter xs
      | searchTarget == "" = xs
      | otherwise = flip filter xs $ \NativeAsset{..} -> or
          [ display policyId == searchTarget
          , display tokenName == searchTarget
          , display policyId <> "." <> display tokenName == searchTarget
          , fingerprint == Fingerprint searchTarget
          , fmap fst (Map.lookup (policyId,tokenName) reverseTickerMap) == Just (Ticker searchTarget)
          ]

    sample :: [NativeAsset]
    sample = keyNftFilter $ searchFilter allAssets

    -- This is the actual asset information.
    assetRow :: NativeAsset -> AppNode
    assetRow a@NativeAsset{..} = do
      let -- How many utxos contain this asset.
          utxoCount = length 
                    $ filter (elem fingerprint . map (view #fingerprint) . view #nativeAssets) 
                    $ wallet ^. #utxos
          loanHistoryEvt = HomeEvent $ InspectCorrespondingLoan (Loans.LoanId tokenName)
          optionsKeyEvt = 
            HomeEvent $ InspectCorrespondingOptionsContract (Options.ContractId tokenName)
      vstack
        [ hstack 
            [ copyableLabelMain (display fingerprint)
                `styleBasic` [textSize 10]
            , widgetIf (policyId == Loans.activeBeaconCurrencySymbol) $ hstack
                [ spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ ("Loan ID: " <> display tokenName) [tooltipDelay 0] $
                      box_ [alignMiddle , onClick loanHistoryEvt] $
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
                , label "Loan Key"
                    `styleBasic` [textSize 10, textFont "Italics", textColor customRed]
                , filler
                ]
            , widgetIf (policyId == Options.activeBeaconCurrencySymbol) $ hstack
                [ spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ ("Options Contract ID: " <> display tokenName) [tooltipDelay 0] $
                      box_ [alignMiddle , onClick optionsKeyEvt] $
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
                , label "Options Key"
                    `styleBasic` [textSize 10, textFont "Italics", textColor customRed]
                , filler
                ]
            , filler
              -- Show the asset name with the ticker if set. Do not use the fingerprint otherwise.
            , label (showAssetBalance False reverseTickerMap a)
                `styleBasic` [textSize 10]
            ]
        , spacer_ [width 2]
        , hstack 
            [ label idCardIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , copyableLabelSub $ display policyId <> "." <> display tokenName
            , filler
            , label (show utxoCount <> " UTxO(s)")
                `styleBasic`
                    [ textFont "Italics"
                    , textSize 8
                    ]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

assetFilterWidget :: AppModel -> AppNode
assetFilterWidget _ = do
  let rootLens = #homeModel % #assetFilterModel
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ bgColor customBlue ]
      choiceButton caption field targetLens =
        optionButton_ caption field targetLens
          [optionButtonOffStyle offStyle]
          `styleBasic` 
            [ bgColor customBlue
            , border 0 transparent
            , textColor white
            , radius 5
            , textSize 12
            ]
  vstack
    [ centerWidget $ vstack
        [ spacer_ [width 30]
        , centerWidgetH $ label "DeFi Filters - Finding Key NFTs for p2p protocols"
        , spacer
        , centerWidgetH $ hstack_ [childSpacing]
            [ label "Key NFT:"
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "Loan" (Just LoanKey) (toLensVL $ rootLens % #keyNftType)
                , choiceButton "Options" (Just OptionsKey) (toLensVL $ rootLens % #keyNftType)
                , choiceButton "Any" Nothing (toLensVL $ rootLens % #keyNftType)
                ]
            , mainButton helpIcon (Alert "Is the asset a Key NFT and if so, what kind?")
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  , padding 0
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ] `styleBasic` [height 30]
        , spacer_ [width 30]
        , hstack 
            [ filler
            , button "Reset" $ HomeEvent ResetAssetFilters
            , spacer
            , toggleButton "Confirm" (toLensVL $ #homeModel % #showAssetFilter)
            ] `styleBasic` [padding 10]
        ] `styleBasic`
            [ bgColor customGray3
            , radius 10
            , border 1 black
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

-------------------------------------------------
-- Helper Widget
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelMain :: Text -> WidgetNode s AppEvent
copyableLabelMain caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 10
      , border 0 transparent
      , textColor customBlue
      , bgColor transparent
      ]
    `styleHover` [textColor white, cursorIcon CursorHand]

-- | A label button that will copy itself.
copyableLabelSub :: Text -> WidgetNode s AppEvent
copyableLabelSub caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize 8
      , border 0 transparent
      , textColor lightGray
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
