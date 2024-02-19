module P2PWallet.GUI.Widgets.Home.Transactions
  ( txWidget
  , txFilterInfo
  ) where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- Calculate the net ADA flux from this address in the transaction.
txValueFromWallet :: PaymentAddress -> Transaction -> ADA
txValueFromWallet addr tx = 
  let isFromAddress x = x ^. paymentAddress == addr
      spent = (sum $ map (toADA . view lovelaces) $ filter isFromAddress $ tx ^. inputs)
            + (sum $ map (toADA . view lovelaces) $ filter isFromAddress $ tx ^. collateralInputs)
      rec = (sum $ map (toADA . view lovelaces) $ filter isFromAddress $ tx ^. outputs)
          + maybe 0 (toADA . view lovelaces) (tx ^. collateralOutput)
  in rec - spent

applyTransactionFilters :: TransactionFilters -> [Transaction] -> [Transaction]
applyTransactionFilters fs ts = do
  t <- ts
  guard $ flip (maybe True) (fs ^. byPolicyId) $ \p ->
    isJust (find (\a -> a ^. policyId == p) $ concatMap (view nativeAssets) $ t ^. inputs) ||
    isJust (find (\a -> a ^. policyId == p) $ concatMap (view nativeAssets) $ t ^. outputs)
  guard $ flip (maybe True) (fs ^. byTokenName) $ \p ->
    isJust (find (\a -> a ^. tokenName == p) $ concatMap (view nativeAssets) $ t ^. inputs) ||
    isJust (find (\a -> a ^. tokenName == p) $ concatMap (view nativeAssets) $ t ^. outputs)
  guard $ flip (maybe True) (fs ^. byFingerprint) $ \p ->
    isJust (find (\a -> a ^. fingerprint == p) $ concatMap (view nativeAssets) $ t ^. inputs) ||
    isJust (find (\a -> a ^. fingerprint == p) $ concatMap (view nativeAssets) $ t ^. outputs)
  return t

txWidget :: AppWenv -> AppModel -> AppNode
txWidget wenv model = 
    vstack 
      [ vscroll_ [wheelRate 50] $ 
          vstack $ map txRow $ reverse $ sortOn (view blockTime) $ 
            applyTransactionFilters (model ^. homeModel . setFilters . txFilters) $
              model ^. homeModel . selectedWallet . txHistory
      , filler
      , hstack
          [ filler
          , customButton wenv "Filter" remixFilter3Line $ 
              HomeEvent $ FilterHomeTransactions StartFiltering
          ] `styleBasic` [bgColor dimGray]
      ]
  where
    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    -- Show the main information for a tx in a box that is clickable. When the box
    -- is clicked, open a more detailed view for that tx.
    txRow :: Transaction -> AppNode
    txRow t =
      let walletAddr = model ^. homeModel . selectedWallet . paymentAddress
          content = 
            hstack 
              [ label_ (t ^. txHash) [resizeFactor 1]
                  `styleBasic` [textFont "Medium", textSize 16]
              , filler
              , vstack 
                  [ filler
                  , hstack 
                      [ filler
                      , label_ 
                          (fromString $ printf "%D ADA" $ 
                            t ^. to (txValueFromWallet walletAddr))
                          [resizeFactor 1]
                      ]
                  , hstack 
                      [ filler
                      , label (showLocalTime "%D %r" $ t ^. blockTime)
                          `styleBasic` [textSize 10]
                      ]
                  ]
              ] `styleBasic` [height 80, padding 20, radius 5]
                `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick (HomeEvent $ ShowHomeDetails $ HomeTransaction t)] content 
          `styleBasic` [padding 10, paddingT 0]

txFilterInfo :: AppWenv -> AppModel -> AppNode
txFilterInfo wenv model = 
  let rootLens = homeModel . newFilters . txFilters
      boolLens' aLens = boolLens "" (rootLens . aLens)
      textLens' aLens = maybeLens "" (rootLens . aLens)
      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "By Policy ID"
              , spacer
              , checkbox_ (boolLens' byPolicyId) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "*" `styleBasic` [textColor crimson]
              , label "Policy ID:"
              , spacer
              , textField (textLens' byPolicyId)
              ] `nodeVisible` (model ^# boolLens' byPolicyId) 
          , hstack 
              [ label "By Token Name"
              , spacer
              , checkbox_ (boolLens' byTokenName) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "*" `styleBasic` [textColor crimson]
              , label "Token Name:"
              , spacer
              , textField (textLens' byTokenName)
              ] `nodeVisible` (model ^# boolLens' byTokenName) 
          , hstack 
              [ label "By Asset Fingerprint"
              , spacer
              , checkbox_ (boolLens' byFingerprint) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "*" `styleBasic` [textColor crimson]
              , label "Fingerprint:"
              , spacer
              , textField (textLens' byFingerprint) 
              ] `nodeVisible` (model ^# boolLens' byFingerprint) 
          ]

  in vstack 
       [ editFields
       , spacer
       , hstack 
           [ mainButton "Reset" $ HomeEvent $ FilterHomeTransactions ResetFiltering
           , filler
           , mainButton "Confirm" $ HomeEvent $ FilterHomeTransactions VerifyFilters
           , spacer
           , button "Cancel" $ HomeEvent $ FilterHomeTransactions CancelFiltering
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
