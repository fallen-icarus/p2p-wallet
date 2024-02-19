module P2PWallet.GUI.Widgets.Delegation.RewardHistory
  ( rewardHistoryWidget
  ) where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.StakeReward
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

rewardHistoryWidget :: AppWenv -> AppModel -> AppNode
rewardHistoryWidget wenv model = 
    vstack 
      [ widgetIf hasRewardHistory $ 
          vscroll_ [wheelRate 50] $ 
            vstack $ map rewardRow history
      , widgetIf (not hasRewardHistory) $ vstack
          [ centerWidget $
              flip styleBasic [bgColor sectionBg, padding 20, radius 5] $ 
                box $ 
                  label "This stake address has not earned any rewards yet."
                   `styleBasic` [textFont "Italics"]
          ]
      ]
  where
    history :: [StakeReward]
    history = model ^. delegationModel . selectedWallet . rewardHistory

    hasRewardHistory :: Bool
    hasRewardHistory = [] /= history
    
    rowBgColor :: Color
    rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def

    sectionBg :: Color
    sectionBg = wenv ^. L.theme . L.sectionColor

    rewardRow :: StakeReward -> AppNode
    rewardRow a =
      vstack
        [
        ]
