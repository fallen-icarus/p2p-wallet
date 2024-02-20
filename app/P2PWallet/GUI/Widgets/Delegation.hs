module P2PWallet.GUI.Widgets.Delegation where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Delegation.Details
import P2PWallet.GUI.Widgets.Delegation.PairStakeWallet
import P2PWallet.GUI.Widgets.Delegation.RegisteredPools
import P2PWallet.GUI.Widgets.Delegation.RewardHistory
import P2PWallet.GUI.Widgets.Delegation.Summary
import P2PWallet.GUI.Widgets.Delegation.WatchStakeWallet
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

delegationWidget :: AppWenv -> AppModel -> AppNode
delegationWidget wenv model = do
    zstack 
      [ mainWidget `nodeVisible` (hasStakeWallets && noDetails && not isAdding)
      , addFirstWalletWidget `nodeVisible` (noDetails && not isAdding && not hasStakeWallets)
      , widgetIf isPairing $ centerWidget $
          box_ [alignCenter] $ vstack
            [ hstack [ filler, label "Pairing", filler ]
            , spacer
            , pairStakeWidget wenv model
            ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , widgetIf isAddingWatched $ centerWidget $
          box_ [alignCenter] $ vstack
            [ hstack [ filler, label "New Watched", filler ]
            , spacer
            , watchStakeWidget wenv model
            ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      , detailsOverlay model `nodeVisible` (not noDetails)
      , widgetIf (model ^. delegationModel . filteringRegisteredPools) $ 
          flip styleBasic [bgColor filterFade] $ 
            centerWidget $
              box_ [alignCenter] $ vstack
                [ hstack [filler, label "Pool Filter Settings", filler]
                , spacer
                , poolFilterInfo wenv model
                ] `styleBasic` [bgColor sectionBg, padding 20, radius 5]
      ]
  where
    sectionBg :: Color
    sectionBg = wenv ^. L.theme . L.sectionColor

    filterFade :: Color
    filterFade = darkGray & L.a .~ 0.8

    reqWalletUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqWalletUpdate _ old new 
      | old ^. wallets . stakeWallets /= new ^. wallets . stakeWallets = True
      | old ^. delegationModel . selectedWallet /= new ^. delegationModel . selectedWallet = True
      | otherwise = False

    reqPoolUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqPoolUpdate _ old new =
      old ^. delegationModel . registeredPools /= new ^. delegationModel . registeredPools

    noDetails :: Bool
    noDetails = isNothing $ model ^. delegationModel . details

    hasStakeWallets :: Bool
    hasStakeWallets = model ^. wallets . stakeWallets /= []

    isPairing :: Bool
    isPairing = model ^. delegationModel . pairing

    isAddingWatched :: Bool
    isAddingWatched = model ^. delegationModel . watching

    isAdding :: Bool
    isAdding = isPairing || isAddingWatched

    isWatched :: Bool
    isWatched = isNothing $ model ^. delegationModel . selectedWallet . stakeKeyPath

    walletTypeIcon :: Text
    walletTypeIcon
      | isWatched = remixEyeLine
      | otherwise = remixLinksLine

    -- The main widget that should only be shown if there are currently tracked payment wallets
    -- AND no details need to be shown.
    mainWidget :: AppNode
    mainWidget =
      vstack
        [ spacer
        , hstack_ [childSpacing] 
            [ filler
            , label $ fromString $ printf "Total Delegation: %D ADA" $ toADA $
                model ^. delegationModel . selectedWallet . totalDelegation
            , separatorLine
            , textDropdown_ 
                  (delegationModel . selectedWallet) 
                  (model ^. wallets . stakeWallets) 
                  (view alias) 
                  []
                `styleBasic` [width 200]
            , label walletTypeIcon 
                `styleBasic` [textFont "Remix",paddingT 15]
            , separatorLine
            , customButton wenv "Refresh Wallets" remixRefreshLine (SyncWallets StartSync)
            , filler
            ] 
        , spacer
        , hgrid_ [childSpacing] 
            [ button "Summary" $ DelegationEvent $ ChangeDelegationScene DelegationSummary
            , button "Reward History" $ DelegationEvent $ ChangeDelegationScene DelegationHistory
            , button "Stake Pools" $ DelegationEvent $ ChangeDelegationScene DelegationPools
            ]
        , spacer
        , box_ [mergeRequired reqWalletUpdate] (summaryWidget wenv model)
            `nodeVisible` (model ^. delegationModel . scene == DelegationSummary)
        , box_ [mergeRequired reqWalletUpdate] (rewardHistoryWidget wenv model)
            `nodeVisible` (model ^. delegationModel . scene == DelegationHistory)
        , box_ [mergeRequired reqPoolUpdate] (registeredPoolsWidget wenv model)
            `nodeVisible` (model ^. delegationModel . scene == DelegationPools)
        ] 

    -- A welcome message when there are currently no tracked stake wallets.
    addFirstWalletWidget :: AppNode
    addFirstWalletWidget =
      vstack
        [ centerWidget $
            flip styleBasic [bgColor sectionBg, padding 20, radius 5] $ 
              box $ 
                label "Add your first stake wallet to begin!" 
                 `styleBasic` [textFont "Italics"]
        , filler
        , hstack
            [ filler
            , box (mainButton "Pair" $ DelegationEvent $ PairStakeWallet StartAdding) 
                `styleBasic` [paddingR 5, paddingB 20, paddingL 20, paddingT 20]
            , box (mainButton "Watch" $ DelegationEvent $ WatchStakeWallet StartAdding) 
                `styleBasic` [paddingL 5, paddingB 20, paddingR 20, paddingT 20]
            ]
        ] `nodeVisible` (not isAdding)
