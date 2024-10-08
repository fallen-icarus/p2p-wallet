module P2PWallet.GUI.Widgets.Delegation.RegisteredPools
  ( 
    registeredPoolsWidget
  , poolFilterInfo
  ) where

import Monomer
import Monomer.Lens qualified as L
import Prettyprinter (tupled,pretty,(<+>))

import P2PWallet.Data.App
import P2PWallet.Data.FilterLang
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

registeredPoolsWidget :: AppWenv -> AppModel -> AppNode
registeredPoolsWidget wenv model =
    vstack
      [ hstack
          [ label ("Registered Pools (" <> show (length sample) <> ")")
              `styleBasic` [paddingL 10, paddingT 10, paddingB 10, paddingR 5]
          , customButton wenv "Refresh Pools" remixRefreshLine (SyncRegisteredPools StartSync)
              `styleBasic` [padding 0]
          , filler
          ]
      , widgetIf (sample == [] && allPools == []) $
          centerWidget spacer
      , widgetIf (sample /= [] && allPools /= []) $ vscroll_ [wheelRate 50] $ 
          vstack $ map poolRow sample
      , widgetIf (sample == [] && allPools /= []) $
          centerWidget $
            flip styleBasic [bgColor sectionBg, padding 20, radius 5] $ 
              box $ 
                label "No pools match those requirements."
                 `styleBasic` [textFont "Italics"]
      , hstack
          [ filler
          , customButton wenv "Filter" remixFilter3Line $ 
              DelegationEvent $ FilterRegisteredPools StartFiltering
          ] `styleBasic` [bgColor dimGray]
      ]
  where
    sectionBg :: Color
    sectionBg = wenv ^. L.theme . L.sectionColor

    filters :: [FilterLang PoolFilterLang]
    filters = unVerifiedPoolFilters $ model ^. delegationModel . setPoolFilters

    allPools :: [Pool]
    allPools = model ^. delegationModel . registeredPools

    sample :: [Pool]
    sample = 
      reverse $ sortOn (view liveSaturation) $
        filter (\p -> poolCheck filters p && isJust (p ^. info)) allPools

    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    -- Show the main information for a pool in a box that is clickable. When the box is clicked,
    -- show additional information for that pool.
    poolRow :: Pool -> AppNode
    poolRow p =
      let evt = DelegationEvent $ ShowDelegationDetails $ DelegationPool p
          pInfo = fromMaybe def $ p ^. info
          nameAndTicker = show $ pretty (pInfo ^. name) <+> tupled [pretty $ pInfo ^. ticker]
          content = 
           vstack
             [ hstack
                 [ label nameAndTicker `styleBasic` [textFont "Medium", textSize 16]
                 , filler
                 , label $ fromString $ 
                     maybe "saturation: none" (printf "saturation: %D%%") $
                       p ^. liveSaturation
                 ]
             , hstack 
                [ filler
                , label $ fromString $
                     maybe "margin: none" (printf "margin: %D%%") $ p ^. margin
                ]
             ] `styleBasic` [height 80, padding 20, radius 5]
               `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick evt] content 
          `styleBasic` [padding 10, paddingT 0]

poolFilterInfo :: AppWenv -> AppModel -> AppNode
poolFilterInfo wenv _ = 
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        scroll $ textArea (delegationModel . newPoolFilters . rawFilters)

  in vstack 
       [ editFields
       , spacer
       , hstack 
           [ mainButton "Reset" $ DelegationEvent $ FilterRegisteredPools ResetFiltering
           , filler
           , mainButton "Confirm" $ DelegationEvent $ FilterRegisteredPools VerifyFilters
           , spacer
           , button "Cancel" $ DelegationEvent $ FilterRegisteredPools CancelFiltering
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]
