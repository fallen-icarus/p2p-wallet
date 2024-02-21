module P2PWallet.GUI.Widgets.Delegation.Details 
  ( detailsOverlay
  ) where

import Monomer

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- A close button in the lower right hand corner of the details page.
detailCloseButton :: PoolID -> WidgetNode s AppEvent
detailCloseButton poolId' =
  vstack_ [childSpacing]
    [ filler
    , hstack 
        [ filler
        , mainButton "Delegate" $ DelegationEvent $ QuickDelegate poolId'
        , spacer
        , button "Close" $ DelegationEvent CloseDelegationDetails 
        , spacer
        , spacer
        ]
    , spacer
    ]

detailTitle :: Text -> WidgetNode s AppEvent
detailTitle title =
  hstack
    [ filler
    , label title `styleBasic` [textSize 20, textFont "Medium"]
    , filler
    ]

detailTextField :: Text -> Text -> WidgetNode s AppEvent
detailTextField l f =
  hstack
    [ spacer
    , spacer
    , label l 
    , copyableTextField f
    ] `styleBasic` [paddingT 0, paddingB 0]

detailsOverlay :: AppModel -> WidgetNode AppModel AppEvent
detailsOverlay model =
    box $ vscroll_ [wheelRate 50] $ 
      case model ^. delegationModel . details of
        Nothing -> copyableTextArea ""
        Just (DelegationPool pool) -> 
          vstack_ [childSpacing] 
            [ spacer
            , detailTitle "Pool Details"
            , spacer
            , detailTextField "Pool ID:" $ toText $ pool ^. poolId
            , detailTextField "Name:" $ view name $ fromMaybe def $ pool ^. info
            , detailTextField "Ticker:" $ view ticker $ fromMaybe def $ pool ^. info
            , detailTextField "Status:" $ showPoolStatus $ pool ^. status
            , detailTextField "Homepage:" $ view homepage $ fromMaybe def $ pool ^. info
            , detailTextField "Description:" $ view description $ fromMaybe def $ pool ^. info
            , spacer
            , detailTextField "Margin:" $ fromString $ 
                maybe "none" (printf "%D%%") $ pool ^. margin
            , detailTextField "Fixed Cost:" $ fromString $ 
                maybe "none" (printf "%D ADA" . toADA) $ pool ^. fixedCost
            , detailTextField "Active Pledge:" $ fromString $ 
                maybe "none" (printf "%D ADA" . toADA) $ pool ^. pledge
            , detailTextField "Active Stake:" $ fromString $ 
                maybe "none" (printf "%D ADA" . toADA) $ pool ^. activeStake
            , detailTextField "Sigma:" $ fromString $ 
                maybe "none" (printf "%D%%") $ pool ^. sigma
            , detailTextField "Total Block Count:" $ maybe "none" show $ pool ^. blockCount
            , detailTextField "Retiring Epoch:" $ maybe "none" show $ pool ^. retiringEpoch
            , detailTextField "Live Pledge:" $ fromString $ 
                maybe "none" (printf "%D ADA" . toADA) $ pool ^. livePledge
            , detailTextField "Live Stake:" $ fromString $ 
                maybe "none" (printf "%D ADA" . toADA) $ pool ^. liveStake
            , detailTextField "Live Delegators:" $ maybe "none" show $ pool ^. liveDelegators
            , detailTextField "Live Saturation:" $ fromString $ 
                maybe "none" (printf "%D%%") $ pool ^. liveSaturation
            , detailCloseButton $ pool ^. poolId
            ]
