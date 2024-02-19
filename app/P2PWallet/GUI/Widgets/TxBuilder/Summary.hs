{-# LANGUAGE DuplicateRecordFields #-}

module P2PWallet.GUI.Widgets.TxBuilder.Summary where

import Monomer
import Prettyprinter
import Data.List ((!!))
import Control.Lens (lens)

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Data.Core
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

summaryWidget :: AppWenv -> AppModel -> AppNode
summaryWidget _ model =
    vscroll $ vstack_ [childSpacing] $ summary <>
      [ filler
      , hstack_ [childSpacing]
          [ filler
          , mainButton "Build" $ TxBuilderEvent BuildTx
          , mainButton "Export" $ TxBuilderEvent $ ChangeBuilderScene BuilderGetExportDestination
          , mainButton "Sign & Submit" SignTx
          , button "Reset" $ TxBuilderEvent ResetBuilder
          , filler
          ]
      , spacer
      ] 
  where
    summary :: [AppNode]
    summary = 
      [ spacer
      , hstack
          [ filler
          , label "Transaction Summary" `styleBasic` [textSize 20, textFont "Medium"]
          , filler
          ]
      , spacer
      , separatorLine
      , inputSummary $ model ^. txBuilderModel . inputs
      , separatorLine
      , outputSummary $ model ^. txBuilderModel . outputs
      , separatorLine
      , certificateSummary $ model ^. txBuilderModel . certificates
      , separatorLine
      , withdrawalSummary $ model ^. txBuilderModel . withdrawals
      , separatorLine
      , changeOutputSummary $ model ^. txBuilderModel . changeOutput
      , separatorLine
      , feeSummary $ toADA $ model ^. txBuilderModel . txFee
      , separatorLine
      ]

inputSummary :: [(Int,VerifiedInput)] -> WidgetNode AppModel AppEvent
inputSummary xs = do
    vstack_ [childSpacing] $
      [ hstack
          [ spacer
          , spacer
          , label "Inputs:"
          , spacer
          , tooltip_ "Add" [tooltipDelay 1000] $
              button remixAddLine (TxBuilderEvent $ ChangeBuilderScene BuilderAddNewInput)
                `styleBasic`
                  [ textFont "Remix"
                  , padding 0
                  , textMiddle
                  , border 0 transparent
                  ]
          ]
      ] <> flip concatMap xs (\(yi,y) ->
             summaryRow "Input" y (yi-1) targetExpanded (DeleteInput yi) (EditInput yi))
  where
    targetExpanded :: Int -> ALens' AppModel Bool
    targetExpanded n = 
      lens (\m -> m ^. txBuilderModel . inputs . to (!!n) . _2 . expanded) 
           (\m b -> 
             let ins = m ^. txBuilderModel . inputs
                 oldTarget = ins !! n 
                 filteredIns = filter (/= oldTarget) ins
                 newTarget = oldTarget & _2 . expanded .~ b
                 newIns = sortOn (view utxoRef . snd) $ newTarget : filteredIns
             in m & txBuilderModel . inputs .~ newIns
           )

outputSummary :: [(Int,VerifiedOutput)] -> WidgetNode AppModel AppEvent
outputSummary xs = do
    vstack_ [childSpacing] $
      [ hstack
          [ spacer
          , spacer
          , label "Outputs:"
          , spacer
          , tooltip_ "Add" [tooltipDelay 1000] $
              button remixAddLine (TxBuilderEvent $ ChangeBuilderScene BuilderAddNewOutput)
                `styleBasic`
                  [ textFont "Remix"
                  , padding 0
                  , textMiddle
                  , border 0 transparent
                  ]
          ]
      ] <> flip concatMap xs (\(yi,y) ->
             summaryRow "Output" y (yi-1) targetExpanded (DeleteOutput yi) (EditOutput yi))
  where
    targetExpanded :: Int -> ALens' AppModel Bool
    targetExpanded n = 
      lens (\m -> m ^. txBuilderModel . outputs . to (!!n) . _2 . expanded) 
           (\m b -> 
             let outs = m ^. txBuilderModel . outputs 
                 oldTarget = outs !! n 
                 filteredOuts = filter (/= oldTarget) outs
                 newTarget = oldTarget & _2 . expanded .~ b
                 newOuts = sortOn fst $ newTarget : filteredOuts
             in m & txBuilderModel . outputs .~ newOuts
           )

certificateSummary :: [(Int,VerifiedCertificate)] -> WidgetNode AppModel AppEvent
certificateSummary xs = do
    vstack_ [childSpacing] $
      [ hstack
          [ spacer
          , spacer
          , label "Certificates:"
          , spacer
          , tooltip_ "Add" [tooltipDelay 1000] $
              button remixAddLine (TxBuilderEvent $ ChangeBuilderScene BuilderAddNewCertificate)
                `styleBasic`
                  [ textFont "Remix"
                  , padding 0
                  , textMiddle
                  , border 0 transparent
                  ]
          ]
      ] <> flip concatMap xs (\(yi,y) ->
             summaryRow 
               "Certificate" 
               y 
               (yi-1) 
               targetExpanded 
               (DeleteCertificate yi) 
               (EditCertificate yi))
  where
    targetExpanded :: Int -> ALens' AppModel Bool
    targetExpanded n = 
      lens (\m -> m ^. txBuilderModel . certificates . to (!!n) . _2 . expanded) 
           (\m b -> 
             let outs = m ^. txBuilderModel . certificates
                 oldTarget = outs !! n 
                 filteredOuts = filter (/= oldTarget) outs
                 newTarget = oldTarget & _2 . expanded .~ b
                 newOuts = sortOn fst $ newTarget : filteredOuts
             in m & txBuilderModel . certificates .~ newOuts
           )

withdrawalSummary :: [(Int,VerifiedWithdrawal)] -> WidgetNode AppModel AppEvent
withdrawalSummary xs = do
    vstack_ [childSpacing] $
      [ hstack
          [ spacer
          , spacer
          , label "Withdrawals:"
          , spacer
          , tooltip_ "Add" [tooltipDelay 1000] $
              button remixAddLine (TxBuilderEvent $ ChangeBuilderScene BuilderAddNewWithdrawal)
                `styleBasic`
                  [ textFont "Remix"
                  , padding 0
                  , textMiddle
                  , border 0 transparent
                  ]
          ]
      ] <> flip concatMap xs (\(yi,y) ->
             summaryRow 
               "Withdrawal" 
               y 
               (yi-1) 
               targetExpanded 
               (DeleteWithdrawal yi) 
               (EditWithdrawal yi))
  where
    targetExpanded :: Int -> ALens' AppModel Bool
    targetExpanded n = 
      lens (\m -> m ^. txBuilderModel . withdrawals . to (!!n) . _2 . expanded) 
           (\m b -> 
             let outs = m ^. txBuilderModel . withdrawals
                 oldTarget = outs !! n 
                 filteredOuts = filter (/= oldTarget) outs
                 newTarget = oldTarget & _2 . expanded .~ b
                 newOuts = sortOn fst $ newTarget : filteredOuts
             in m & txBuilderModel . withdrawals .~ newOuts
           )

feeSummary :: ADA -> WidgetNode AppModel AppEvent
feeSummary n =
  hstack
    [ spacer
    , spacer
    , label $ fromString $ printf "Fee: %D ADA" n
    ]

changeOutputSummary :: VerifiedChangeOutput -> WidgetNode AppModel AppEvent
changeOutputSummary m =
  vstack_ [childSpacing] $
    [ hstack
        [ spacer
        , spacer
        , label "Change Output:"
        , spacer
        , tooltip_ "Edit" [tooltipDelay 1000] $
            button remixEdit2Line (TxBuilderEvent EditChangeOutput)
              `styleBasic`
                [ textFont "Remix"
                , padding 0
                , textMiddle
                , border 0 transparent
                ]
        ]
    , hstack
        [ spacer
        , spacer
        , spacer
        , spacer
        , copyableTextArea (show $ pretty m)
            `styleBasic` [paddingL 0]
        ] `nodeVisible` (m ^. expanded)
    ]

-------------------------------------------------
-- Helpers
-------------------------------------------------
summaryTextField :: Text -> Text -> WidgetNode s AppEvent
summaryTextField header f =
  hstack
    [ spacer
    , spacer
    , label header
    , copyableTextField f
    ] `styleBasic` [paddingT 0, paddingB 0]

summaryRow 
  :: (HasExpanded a Bool, Pretty a) 
  => Text 
  -> a 
  -> Int
  -> (Int -> ALens' AppModel Bool)
  -> TxBuilderEvent -- DeleteEvent
  -> TxBuilderEvent -- EditEvent
  -> [WidgetNode AppModel AppEvent]
summaryRow subHeader x n aLens deleteEvt editEvt = 
    [ hstack
        [ spacer
        , spacer
        , spacer
        , spacer
        , label $ subHeader <> " " <> show (n + 1)
        , spacer
        , toggleButton remixMoreLine (aLens n)
            `styleHover` [cursorIcon CursorHand]
            `styleBasic` 
              [ textFont "Remix"
              , padding 0
              , textMiddle
              , border 0 transparent
              ]
        , spacer
        , tooltip_ "Edit" [tooltipDelay 1000] $
            button remixEdit2Line (TxBuilderEvent editEvt)
              `styleBasic`
                [ textFont "Remix"
                , padding 0
                , textMiddle
                , border 0 transparent
                ]
        , spacer
        , tooltip_ "Remove" [tooltipDelay 1000] $
            button remixDeleteBinLine (TxBuilderEvent deleteEvt)
              `styleBasic`
                [ textFont "Remix"
                , padding 0
                , textMiddle
                , border 0 transparent
                ]
        ]
    , hstack
        [ spacer
        , spacer
        , spacer
        , spacer
        , spacer
        , spacer
        , copyableTextArea (show $ pretty x)
        ] `nodeVisible` (x ^. expanded)
    ] 
