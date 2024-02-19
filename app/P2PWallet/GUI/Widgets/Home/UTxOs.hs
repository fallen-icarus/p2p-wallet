module P2PWallet.GUI.Widgets.Home.UTxOs 
  ( utxoWidget
  , utxoFilterInfo
  ) where

import Monomer
import Monomer.Lens qualified as L
import Data.Text qualified as T

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

applyAddressUTxOFilters :: UTxOFilters -> [AddressUTxO] -> [AddressUTxO]
applyAddressUTxOFilters fs us = do
  u <- us
  guard $ maybe True (flip hasAssetWithPolicyId u) (fs ^. byPolicyId)
  guard $ maybe True (flip hasAssetWithTokenName u) (fs ^. byTokenName)
  guard $ maybe True (flip hasAssetWithFingerprint u) (fs ^. byFingerprint)
  guard $ flip (maybe True) (fs ^. byReferenceScriptHash) $ \r ->
    if r == ""
    then isJust $ u ^. referenceScriptHash
    else Just r == u ^. referenceScriptHash
  guard $ flip (maybe True) (fs ^. byDatumHash) $ \r ->
    if r == ""
    then isJust $ u ^. datumHash
    else Just r == u ^. datumHash
  guard $ flip (maybe True) (fs ^. byTxHash) $ 
    \hash -> hash == T.takeWhile (/='#') (showTxOutRef $ u ^. utxoRef)
  return u

utxoWidget :: AppWenv -> AppModel -> AppNode
utxoWidget wenv model =
  vstack
    [ label ("UTxOs (" <> show (length sample) <> ")")
        `styleBasic` [padding 10]
    , vscroll_ [wheelRate 50] $ 
        vstack $ map utxoRow $ sortOn (view utxoRef) sample
    , filler
    , hstack
        [ filler
        , customButton wenv "Filter" remixFilter3Line $ HomeEvent $ FilterHomeUTxOs StartFiltering
        ] `styleBasic` [bgColor dimGray]
    ]
  where
    sample :: [AddressUTxO]
    sample = applyAddressUTxOFilters (model ^. homeModel . setFilters . utxoFilters) $
      model ^. homeModel . selectedWallet . utxos

    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    -- Show the main information for a utxo in a box that is clickable. When the box
    -- is clicked, open a more detailed view for that utxo.
    utxoRow :: AddressUTxO -> AppNode
    utxoRow u =
      let content = 
            hstack 
              [ label_ (showTxOutRef $ u ^. utxoRef) [resizeFactor 1]
                  `styleBasic` [textFont "Medium", textSize 16]
              , filler
              , label_ (toText @String $ printf "%D ADA" $ toADA $ u ^. lovelaces) 
                  [resizeFactor 1]
              ] `styleBasic` [height 80, padding 20, radius 5]
                `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick (HomeEvent $ ShowHomeDetails $ HomeUTxO u)] content 
          `styleBasic` [padding 10, paddingT 0]

utxoFilterInfo :: AppWenv -> AppModel -> AppNode
utxoFilterInfo wenv model = 
  let rootLens = homeModel . newFilters . utxoFilters
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
          , hstack 
              [ label "By Reference Script"
              , spacer
              , checkbox_ (boolLens' byReferenceScriptHash) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "Reference Script Hash (leave empty to match any):"
              , spacer
              , textField (textLens' byReferenceScriptHash) 
              ] `nodeVisible` (model ^# boolLens' byReferenceScriptHash) 
          , hstack 
              [ label "By Datum"
              , spacer
              , checkbox_ (boolLens' byDatumHash) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "Datum Hash (leave empty to match any):"
              , spacer
              , textField (textLens' byDatumHash) 
              ] `nodeVisible` (model ^# boolLens' byDatumHash) 
          , hstack 
              [ label "By Tx Hash"
              , spacer
              , checkbox_ (boolLens' byTxHash) [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "*" `styleBasic` [textColor crimson]
              , label "Tx Hash:"
              , spacer
              , textField (textLens' byTxHash)
              ] `nodeVisible` (model ^# boolLens' byTxHash) 
          ]

  in vstack 
       [ editFields
       , spacer
       , hstack 
           [ mainButton "Reset" $ HomeEvent $ FilterHomeUTxOs ResetFiltering
           , filler
           , mainButton "Confirm" $ HomeEvent $ FilterHomeUTxOs VerifyFilters
           , spacer
           , button "Cancel" $ HomeEvent $ FilterHomeUTxOs CancelFiltering
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
