module P2PWallet.GUI.Widgets.Home.Assets
  ( assetWidget
  , assetFilterInfo
  ) where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

applyAssetFilters :: AssetFilters -> [NativeAsset] -> [NativeAsset]
applyAssetFilters fs as = do
  a <- as
  guard $ maybe True (== a ^. policyId) (fs ^. byPolicyId)
  guard $ maybe True (== a ^. tokenName) (fs ^. byTokenName)
  guard $ maybe True (== a ^. fingerprint) (fs ^. byFingerprint)
  return a

assetWidget :: AppWenv -> AppModel -> AppNode
assetWidget wenv model = 
    vstack 
      [ widgetIf hasAssets $ vscroll_ [wheelRate 50] $ 
          vstack $ map assetRow $ reverse $ sortOn (view fingerprint) $ 
            applyAssetFilters (model ^. homeModel . setFilters . assetFilters) $
              model ^. homeModel . selectedWallet . nativeAssets
      , widgetIf (not hasAssets) $
          centerWidget $
            flip styleBasic [bgColor sectionBg, padding 20, radius 5] $ 
              box $ 
                label "This address does not have any native assets."
                 `styleBasic` [textFont "Italics"]
      , filler
      , hstack
          [ filler
          , customButton wenv "Filter" remixFilter3Line $ 
              HomeEvent $ FilterHomeAssets StartFiltering
          ] `styleBasic` [bgColor dimGray]
      ]
  where
    sectionBg :: Color
    sectionBg = wenv ^. L.theme . L.sectionColor

    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    hasAssets :: Bool
    hasAssets = [] /= model ^. homeModel . selectedWallet . nativeAssets

    -- Show the main information for an asset in a box that is clickable. When the box
    -- is clicked, open a more detailed view for that asset.
    assetRow :: NativeAsset -> AppNode
    assetRow a =
      let content = 
            hstack 
              [ vstack 
                  [ filler
                  , label (a ^. fingerprint) `styleBasic` [textFont "Medium", textSize 16]
                  , label (a ^. to fullAssetName) `styleBasic` [textSize 12]
                  ]
              , filler
              , label (show $ a ^. quantity) 
              ] `styleBasic` [height 80, padding 20, radius 5]
                `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick (HomeEvent $ ShowHomeDetails $ HomeAsset a)] content 
           `styleBasic` [padding 10, paddingT 0]

assetFilterInfo :: AppWenv -> AppModel -> AppNode
assetFilterInfo wenv model = 
  let rootLens = homeModel . newFilters . assetFilters
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
           [ mainButton "Reset" $ HomeEvent $ FilterHomeAssets ResetFiltering
           , filler
           , mainButton "Confirm" $ HomeEvent $ FilterHomeAssets VerifyFilters
           , spacer
           , button "Cancel" $ HomeEvent $ FilterHomeAssets CancelFiltering
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
