{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Delegation.Summary where

import Monomer
import Monomer.Lens qualified as L
import Prettyprinter (tupled,pretty,(<+>))

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.Data.Wallets.StakeWallet
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

summaryWidget :: AppWenv -> AppModel -> AppNode
summaryWidget wenv model = do
    vstack 
      [ hstack [spacer, addressInfoWidget wenv model, spacer]
      , spacer
      , hstack [spacer, balanceWidget wenv model, spacer]
      , filler
      , hstack
          [ filler
          , box (mainButton "Pair" $ DelegationEvent $ PairStakeWallet StartAdding) 
              `styleBasic` [paddingR 5, paddingB 20, paddingL 20, paddingT 20]
          , box (mainButton "Watch" $ DelegationEvent $ WatchStakeWallet StartAdding) 
              `styleBasic` [paddingL 5, paddingB 20, paddingR 20, paddingT 20]
          ]
      ] `nodeVisible` (not isAdding)

  where
    isPairing :: Bool
    isPairing = model ^. delegationModel . pairing

    isAddingWatched :: Bool
    isAddingWatched = model ^. delegationModel . watching

    isAdding :: Bool
    isAdding = isPairing || isAddingWatched

-- | Show available rewards and current delegation information.
balanceWidget :: AppWenv -> AppModel -> AppNode
balanceWidget wenv model = do
    hstack 
      [ rewardsWidget `styleBasic` [radius 5, bgColor sectionBgColor]
      , spacer 
      , delegationWidget `styleBasic` [radius 5, bgColor sectionBgColor]
      ]

  where
    sectionBgColor = wenv ^. L.theme . L.sectionColor

    wallet :: StakeWallet
    wallet = model ^. delegationModel . selectedWallet

    registered :: Bool
    registered = wallet ^. registrationStatus /= NotRegistered

    delegationWidget :: AppNode
    delegationWidget = centerWidget $
      vstack
        [ widgetIf (not registered) $
            vstack
              [ centerWidgetH $ label "Not Registered"
                  `styleBasic` [textFont "Medium", textSize 24]
              , spacer
              , centerWidgetH $ 
                  label "Register the credential to enable delegation and reward withdrawals."
              , spacer
              , centerWidgetH $ mainButton "Register" $ DelegationEvent QuickRegister
              ]
        -- , widgetIf (wallet ^. registrationStatus == NotRegistered) $ centerWidgetH $
        --     vstack
        --       [ flip styleBasic [textSize 14] $ label $ 
        --           show $ tupled $ ["register to enable withdrawals"]
        --       , spacer
        --       , mainButton "Register" $ DelegationEvent QuickRegister
        --       ]
        , widgetIf (registered && isNothing (wallet ^. delegatedPool)) $
            vstack
              [ centerWidgetH $ label "Not Delegated"
                  `styleBasic` [textFont "Medium", textSize 24]
              , spacer
              , centerWidgetH $ label "Delegate to start earning rewards!"
              ]
        , widgetMaybe (wallet ^. delegatedPool) $ \pool ->
            let pInfo = fromMaybe def $ pool ^. info
                nameAndTicker = show $ pretty (pInfo ^. name) <+> tupled [pretty $ pInfo ^. ticker]
            in vstack
              [ centerWidgetH $ label nameAndTicker `styleBasic` [textFont "Medium", textSize 24]
              , spacer
              , centerWidgetH $ flip styleBasic [textFont "Medium", textSize 16] $ 
                  label $ fromString $ 
                    maybe "Live Saturation: none" (printf "Live Saturation: %D%%") $ 
                      pool ^. liveSaturation
              , spacer
              , centerWidgetH $ flip styleBasic [textFont "Medium", textSize 16] $ 
                  label $ fromString $ maybe "Margin: none" (printf "Margin: %D%%") $ 
                    pool ^. margin
              , spacer
              , centerWidgetH $ flip styleBasic [textFont "Medium", textSize 16] $ 
                  label $ fromString $ 
                    maybe "Fixed Cost: none" (printf "Fixed Cost: %D ADA" . toADA) $ 
                      pool ^. fixedCost
              , spacer
              ] `styleBasic` [width 600]
        ]

    rewardsWidget :: AppNode
    rewardsWidget = centerWidget $
      vstack
        [ centerWidgetH $
            label "Available Rewards:" `styleBasic` [textFont "Medium", textSize 24]
        , spacer
        , centerWidgetH $ 
            flip styleBasic [textFont "Medium", textSize 20] $ label $ 
              fromString $ printf "%D ADA" $ toADA $ wallet ^. availableRewards
        , widgetIf registered $ centerWidgetH $
            vstack
              [ spacer
              , mainButton "Withdraw" $ DelegationEvent QuickWithdraw
              ]
        , widgetIf (not registered) $ centerWidgetH $
            flip styleBasic [textSize 14] $ label $ 
              show $ tupled $ ["register to enable withdrawals"]
        ]

-- | Show key information about the address.
addressInfoWidget :: AppWenv -> AppModel -> AppNode
addressInfoWidget wenv model = do
  let wallet = model ^. delegationModel . selectedWallet
      eInfo = inspectBech32Address $ unStakeAddress $ wallet ^. stakeAddress
      stakeKeyHash = either (const Nothing) infoStakeKeyHash eInfo
      sectionBgColor = wenv ^. L.theme . L.sectionColor

  vstack 
    [ centerWidgetH $ label "Address Info" 
        `styleBasic` [textFont "Italics", textSize 18, padding 10]
    , vstack
        [ hstack
            [ spacer
            , spacer
            , label "Stake Credential" `styleBasic` [textFont "Italics"]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Stake Address:"
            , spacer
            , copyableTextArea $ toText $ wallet ^. stakeAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Stake Key Hash:" 
            , spacer
            , flip styleBasic [width 550] $ copyableTextArea $ 
                fromMaybe "none" $ show . PubKeyHash . BuiltinByteString <$> stakeKeyHash
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Derivation Path:"
            , spacer
            , copyableTextArea $ fromMaybe "none" $
                show . pretty . showDerivationPath <$> wallet ^. stakeKeyPath
            ] `styleBasic` [padding 5]
        ]
    , spacer
    ] `styleBasic` [radius 5, bgColor sectionBgColor]
