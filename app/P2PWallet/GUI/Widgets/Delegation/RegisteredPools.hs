module P2PWallet.GUI.Widgets.Delegation.RegisteredPools
  ( 
    registeredPoolsWidget
  ) where

import Monomer
import Monomer.Lens qualified as L
import Prettyprinter (tupled,pretty,(<+>))

import P2PWallet.Data.App
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
    , vscroll_ [wheelRate 50] $ 
        vstack $ map poolRow sample
    ]
  where
    sample :: [Pool]
    sample = 
      reverse $ sortOn (view liveSaturation) $ 
        filter (isJust . view info) $ model ^. registeredPools

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
                 , label $ toText @String $ 
                     maybe "saturation: none" (printf "saturation: %D%%") $
                       p ^. liveSaturation
                 ]
             , hstack 
                [ filler
                , label $ toText @String $
                     maybe "margin: none" (printf "margin: %D%%") $ p ^. margin
                ]
             ] `styleBasic` [height 80, padding 20, radius 5]
               `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      in box_ [expandContent, onClick evt] content 
          `styleBasic` [padding 10, paddingT 0]
