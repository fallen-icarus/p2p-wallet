{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Delegation
  ( 
    delegationWidget
  ) where

import Monomer hiding (icon)
import Prettyprinter ((<+>), pretty, tupled)
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.StakeReward
import P2PWallet.Data.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Delegation.AddStakeWallet
import P2PWallet.GUI.Widgets.Delegation.PoolPicker
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Prelude

delegationWidget :: AppModel -> AppNode
delegationWidget model = do
    zstack
      [ mainWidget model 
          `nodeVisible` and
            [ hasStakeWallets
            , not isAdding
            , not isEditing
            , not isDeleting
            ]
      , addFirstWalletWidget 
          `nodeVisible` (not isAdding && not hasStakeWallets && not isEditing && not isDeleting)
      , widgetIf isAdding $ addStakeWalletWidget model
      , widgetIf isEditing $ editStakeWalletWidget model
      , widgetIf isDeleting $ confirmDeleteWidget model
      , widgetIf (model ^. #delegationModel % #showPoolPicker) $ poolPickerWidget model
      , widgetIf (model ^. #delegationModel % #showPoolFilter) $ poolFilterWidget model
      ]
  where
    hasStakeWallets :: Bool
    hasStakeWallets = model ^. #knownWallets % #stakeWallets /= []

    isAdding :: Bool
    isAdding = model ^. #delegationModel % #addingWallet

    isEditing :: Bool
    isEditing = model ^. #delegationModel % #editingWallet

    isDeleting :: Bool
    isDeleting = model ^. #delegationModel % #deletingWallet

    -- A welcome message when there are currently no tracked stake wallets.
    addFirstWalletWidget :: AppNode
    addFirstWalletWidget =
      vstack
        [ centerWidget $
            flip styleBasic [bgColor transparent, padding 20, radius 5] $ 
              box $ 
                label "Add your first stake wallet to begin!" 
                 `styleBasic` [textFont "Italics"]
        , filler
        , hstack
            [ filler
            -- The widget is initialized using PairStakeWallet. It is the same for all wallet 
            -- types.
            , box (mainButton "Add Wallet" $ DelegationEvent $ PairStakeWallet $ StartAdding Nothing) 
                `styleBasic` [padding 20]
            ]
        ] `nodeVisible` (not isAdding)

-- The main widget that should only be shown if there are currently tracked stake wallets
-- AND no overlays need to be shown.
mainWidget :: AppModel -> AppNode
mainWidget model =
    vstack
      [ spacer
      , centerWidgetH headerWidget
      , spacer
      , hgrid
          [ vstack
              [ centerWidgetH $ hstack
                  [ registrationStatusWidget
                  , spacer
                  , totalDelegatedWidget
                  , spacer
                  , rewardsBalanceWidget
                  ]
              , spacer
              , widgetMaybe (wallet ^. #delegatedPool) poolInfoWidget
              , widgetIf (isNothing $ wallet ^. #delegatedPool) notDelegatedWidget
              ] `styleBasic` [padding 10]
          , vstack
              [ vstack 
                  [ centerWidgetH $ label "Reward History"
                      `styleBasic` [textFont "Italics", textSize 14, paddingT 10]
                  , separatorLine `styleBasic` [paddingL 100, paddingR 100, fgColor darkGray]
                  , widgetIf (rewardHistory /= []) historyTableWidget
                  , widgetIf (rewardHistory == []) $ centerWidget $ 
                      flip styleBasic [padding 10, bgColor customGray4, radius 10] $ box $ 
                        label "This staking address has not earned any rewards yet."
                          `styleBasic` [textSize 12]
                  ] `styleBasic` 
                      [ bgColor customGray2, radius 15
                      , height 360
                      ]
              ] `styleBasic` [padding 10]
          ]
      , flip styleBasic [padding 10] $ box $ vstack
          [ centerWidgetH $ hstack
              [ label "Active Linked Payment Addresses"
                  `styleBasic` [textFont "Italics", textSize 14]
              , mainButton helpIcon (Alert activeLinkedAddressesMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , padding 2
                    , textSize 12
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , separatorLine `styleBasic` [paddingL 175, paddingR 175, fgColor darkGray]
          , spacer
          , widgetIf(linkedAddresses == []) $ centerWidget $ 
              flip styleBasic [padding 10, bgColor customGray4, radius 10] $ box $ 
                label "There are no active linked payment addresses."
                  `styleBasic` [textSize 12, textColor lightGray]
          , widgetIf(linkedAddresses /= []) $
              vscroll_ [scrollOverlay, wheelRate 50, barWidth 3, thumbWidth 3] $ 
                vstack_ [childSpacing_ 1] $ 
                  flip map linkedAddresses $ \addr ->
                    centerWidgetH $ copyableLabelSelf 12 lightGray (toText addr)
                      `styleBasic` [radius 20, padding 5, bgColor customGray4]
          ] `styleBasic` [radius 15, bgColor customGray2, padding 10]
      ] `styleBasic` [padding 10]
  where
    wallet :: StakeWallet
    wallet@StakeWallet{registrationStatus,rewardHistory,linkedAddresses} = 
      model ^. #delegationModel % #selectedWallet

    -- Shows an icon representing where the address is paired or watched.
    walletTypeLabel :: AppNode
    walletTypeLabel
      | isNothing (wallet ^. #stakeKeyPath) = 
          tooltip_ "Watched" [tooltipDelay 0] $ label watchedIcon
            `styleBasic` [textColor customBlue, textMiddle, textFont "Remix"]
      | otherwise = 
          tooltip_ "Paired" [tooltipDelay 0] $ label pairedIcon
            `styleBasic` [textColor customBlue, textMiddle, textFont "Remix"]

    historyTableWidget :: AppNode
    historyTableWidget = do
      vstack
        [ hgrid_ [childSpacing]
            [ centerWidgetH $ hstack
                [ label "Epoch" `styleBasic` [textSize 12]
                , mainButton helpIcon (Alert spendableEpochMsg)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , padding 2
                      , textSize 10
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ]
            , centerWidgetH $ label "Rewards" `styleBasic` [textSize 12]
            , centerWidgetH $ label "Pool ID" `styleBasic` [textSize 12]
            ] `styleBasic` [padding 5, bgColor customGray4, radiusTL 5, radiusTR 5]
        , spacer_ [width 1]
        , vscroll_ [scrollOverlay, wheelRate 50, thumbWidth 3] $ vstack_ [childSpacing_ 1] $ 
            flip map (take 50 rewardHistory) $ \StakeReward{..} ->
              hgrid_ [childSpacing]
                [ centerWidgetH $ label (show spendableEpoch) 
                    `styleBasic` [textSize 12, textColor lightGray]
                , centerWidgetH $ label (fromString $ printf "%D ADA" $ toAda amount) 
                    `styleBasic` [textSize 12, textColor lightGray]
                , centerWidgetH $ copyableTruncatedPoolId 12 lightGray poolId
                ] `styleBasic` [padding 5, bgColor customGray4]
        ] `styleBasic` [ padding 10 ]

    changeDelegationButton :: AppNode
    changeDelegationButton = do
      tooltip_ "Change Delegation" [tooltipDelay 0] $ 
        box_ [onClick $ DelegationEvent OpenPoolPicker] $ 
          label "Change"
            `styleBasic`
              [ textSize 10
              , bgColor customBlue
              , padding 3
              , textMiddle
              , radius 10
              , textColor lightGray
              ]
            `styleHover`
              [ bgColor customGray1 
              , cursorIcon CursorHand
              ]

    poolInfoWidget :: Pool -> AppNode
    poolInfoWidget Pool{..} = do
      let PoolInfo{..} = fromMaybe def info
          nameAndTicker = show $ pretty name <+> tupled [pretty ticker]
      vstack
        [ hstack
            [ label nameAndTicker `styleBasic` [textSize 14]
            , filler
            , changeDelegationButton
            ]
        , spacer_ [width 5]
        , copyableLabelSelf 10 lightGray (toText poolId)
        , spacer_ [width 3]
        , copyableLabelSelf 10 lightGray homepage
        , spacer
        , widgetMaybe retiringEpoch $ \epoch ->
            vstack
              [ label ("WARNING: This pool is retiring on epoch " <> show epoch)
                  `styleBasic`
                      [textFont "Italics", textSize 12, textColor customRed]
              , spacer
              ]
        , hgrid_ [childSpacing]
            [ subField "Margin" marginMsg $ 
                fromString $ printf "%D%%" $ (*100) $ fromMaybe 0 margin
            , subField "Live Saturation" liveSaturationMsg $ 
                fromString $ printf "%D%%" $ fromMaybe 0 liveSaturation
            ]
        , spacer
        , subField "Pledge" pledgeMsg $
            fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 pledge
        , spacer
        , subField "Active Pledge" activePledgeMsg $
            fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 livePledge
        , spacer
        , subField "Cost" fixedCostMsg $
            fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 fixedCost
        ] `styleBasic`
            [ bgColor customGray2
            , padding 10
            , radius 10
            , height 300
            ]

    notDelegatedWidget :: AppNode
    notDelegatedWidget = do
      flip styleBasic [height 300, bgColor customGray2, padding 10, radius 10] $ box $ 
        centerWidget $ vstack
          [ centerWidgetH $ label "Delegate to start earning rewards!"
              `styleBasic` [ textFont "Italics" ]
          , widgetIf (registrationStatus == NotRegistered) $
              vstack
                [ spacer_ [width 3]
                , centerWidgetH $ label (show $ tupled ["Don't forget to also register!"])
                    `styleBasic` [ textColor customRed, textSize 12 ]
                ]
          , spacer
          , centerWidgetH $ mainButton "Delegate" $ DelegationEvent OpenPoolPicker
          ]

    withdrawButton :: AppNode
    withdrawButton = do
      let (tip,mainColor,highlightColor,event)
            | registrationStatus == Registered = ("Withdraw",customBlue,customGray1,AppInit)
            | otherwise = ("Register to enable withdrawals",customRed,transparent,AppInit)
      tooltip_ tip [tooltipDelay 0] $ box_ [onClick event] $ 
        label withdrawRewardsIcon
          `styleBasic`
            [ textColor mainColor 
            , textSize 10
            , textFont "Remix"
            , bgColor customGray2
            , padding 2
            , textMiddle
            , radius 10
            ]
          `styleHover`
            [ bgColor highlightColor
            , cursorIcon CursorHand
            ]

    subField :: Text -> Text -> Text -> AppNode
    subField caption helpMsg field =
      vstack
        [ hstack
            [ label caption
                `styleBasic`
                  [ textSize 10
                  , textColor lightGray
                  ]
            , subTipButton helpMsg
            ]
        , spacer_ [width 3]
        , label field
            `styleBasic`
              [ textSize 12
              ]
        ] `styleBasic`
            [ bgColor customGray4
            , padding 10
            , radius 10
            ]

    mainTipButton :: Text -> AppNode
    mainTipButton msg =
      box_ [onClick $ Alert msg] $ 
        label helpIcon
          `styleBasic`
            [ textColor customBlue 
            , textSize 10
            , textFont "Remix"
            , bgColor customGray2
            , padding 2
            , textMiddle
            , radius 10
            ]
          `styleHover`
            [ bgColor customGray1 
            , cursorIcon CursorHand
            ]

    subTipButton :: Text -> AppNode
    subTipButton msg =
      box_ [onClick $ Alert msg] $ 
        label helpIcon
          `styleBasic`
            [ textColor customBlue 
            , textSize 10
            , textFont "Remix"
            , bgColor customGray4
            , padding 2
            , textMiddle
            , radius 10
            ]
          `styleHover`
            [ bgColor customGray2 
            , cursorIcon CursorHand
            ]

    registrationButton :: AppNode
    registrationButton = do
      let (tip,icon)
            | registrationStatus == NotRegistered = ("Register",registerIcon)
            | otherwise = ("Deregister",deregisterIcon)
      tooltip_ tip [tooltipDelay 0] $ box_ [onClick AppInit] $ 
        label icon
          `styleBasic`
            [ textSize 10
            , textFont "Remix"
            , bgColor customGray2
            , padding 2
            , textMiddle
            , radius 10
            , if registrationStatus == NotRegistered then 
                textColor customBlue 
              else 
                textColor customRed
            ]
          `styleHover`
            [ bgColor customGray1 
            , cursorIcon CursorHand
            ]

    registrationStatusWidget :: AppNode
    registrationStatusWidget = do
      vstack
        [ hstack
            [ label "Status"
                `styleBasic`
                  [ textSize 10
                  , textColor lightGray
                  ]
            , registrationButton
            ]
        , spacer_ [width 3]
        , label (displayRegistrationStatus registrationStatus)
            `styleBasic`
              [ textSize 11
              , if registrationStatus == NotRegistered then 
                  textColor customRed 
                else 
                  textColor customBlue
              ]
        ] `styleBasic`
            [ bgColor customGray2
            , padding 10
            , radius 10
            ]

    totalDelegatedWidget :: AppNode
    totalDelegatedWidget = do
      vstack
        [ hstack
            [ label "Total Delegated"
                `styleBasic`
                  [ textSize 10
                  , textColor lightGray
                  ]
            , mainTipButton totalDelegatedMsg
            ]
        , spacer_ [width 3]
        , label (fromString $ printf "%D ADA" $ toAda $ wallet ^. #totalDelegation)
            `styleBasic`
              [ textSize 11
              ]
        ] `styleBasic`
            [ bgColor customGray2
            , padding 10
            , radius 10
            ]

    rewardsBalanceWidget :: AppNode
    rewardsBalanceWidget = do
      vstack
        [ hstack
            [ label "Rewards Balance"
                `styleBasic`
                  [ textSize 10
                  , textColor lightGray
                  ]
            , withdrawButton
            ]
        , spacer_ [width 3]
        , label (fromString $ printf "%D ADA" $ toAda $ wallet ^. #availableRewards)
            `styleBasic`
              [ textSize 11
              ]
        ] `styleBasic`
            [ bgColor customGray2
            , padding 10
            , radius 10
            ]

    headerWidget :: AppNode
    headerWidget = do
      let innerDormantStyle = 
            def `styleBasic` [bgColor customGray3, border 1 black]
                `styleHover` [bgColor customGray2, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [bgColor customGray3, border 1 customBlue]
                `styleFocusHover` [bgColor customGray2, border 1 customBlue]
      hstack
        [ hstack
            [ walletTypeLabel
            , spacer_ [width 5]
            , widgetMaybe (wallet ^. #stakeKeyPath) $ \path ->
                vstack
                  [ copyableLabelSelf 12 white (toText $ wallet ^. #stakeAddress)
                  , copyableLabelSelf 10 lightGray $ showDerivationPath path
                  ]
            , widgetIf (isNothing $ wallet ^. #stakeKeyPath) $
                copyableLabelSelf 12 white (toText $ wallet ^. #stakeAddress)
            , spacer_ [width 1]
            , morePopup model `styleBasic` [styleIf (isJust $ wallet ^. #stakeKeyPath) $ paddingT 2]
            ] `styleBasic`
                [ bgColor customGray3
                , padding 10
                , radius 10
                , border 1 black
                ]
        , spacer
        , textDropdown_ 
              (toLensVL $ #delegationModel % #selectedWallet) 
              (model ^. #knownWallets % #stakeWallets) 
              (view #alias) 
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray3
              , width 150
              , paddingR 10
              , border 1 black
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer
        , tooltip_ "Refresh" [tooltipDelay 0] $
            button refreshIcon (SyncWallets StartSync)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 0
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        ]

morePopup :: AppModel -> AppNode
morePopup _ = do
  vstack
    [ tooltip_ "More" [tooltipDelay 0] $
        button verticalMoreIcon (DelegationEvent ShowDelegationMorePopup)
          `styleBasic`
            [ border 0 transparent
            , padding 0
            , bgColor transparent
            , textColor customBlue
            , textMiddle
            , textFont "Remix"
            ]
          `styleHover` [bgColor customGray1, cursorIcon CursorHand]
    , customPopup (toLensVL $ #delegationModel % #showMorePopup) $
        vstack
          [ button "Add Wallet" (DelegationEvent $ PairStakeWallet $ StartAdding Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , button "Edit Name" (DelegationEvent $ ChangeStakeWalletName $ StartAdding Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , separatorLine `styleBasic` [fgColor black, padding 5]
          , button "Delete Wallet" (DelegationEvent $ DeleteStakeWallet $ GetDeleteConfirmation Nothing)
              `styleBasic`
                [ border 0 transparent
                , textSize 12
                , bgColor transparent
                , textColor customRed
                , textMiddle
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          ] `styleBasic`
              [ bgColor customGray3
              , border 1 black
              , padding 5
              ]
    ] `styleBasic` [padding 0, height 5]

editStakeWalletWidget :: AppModel -> AppNode
editStakeWalletWidget _ = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Wallet Name:"
            , spacer
            , textField (toLensVL $ #extraTextField) 
                `styleBasic` [width 500]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DelegationEvent $ ChangeStakeWalletName CancelAdding
        , spacer
        , mainButton "Confirm" $ DelegationEvent $ ChangeStakeWalletName ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]


confirmDeleteWidget :: AppModel -> AppNode
confirmDeleteWidget model = do
  centerWidget $ vstack_ [childSpacing]
    [ spacer
    , centerWidgetH $ label $ mconcat
        [ "Are you sure you would like to delete '"
        , model ^. #delegationModel % #selectedWallet % #alias
        , "'?"
        ]
    , hstack 
        [ filler
        , button "Cancel" $ DelegationEvent $ DeleteStakeWallet CancelDeletion
        , spacer
        , mainButton "Confirm" $ DelegationEvent $ DeleteStakeWallet ConfirmDeletion
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, width 700]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Double -> Color -> Text -> WidgetNode s AppEvent
copyableLabelSelf fontSize mainColor caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , textLeft
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy itself but display a truncated version.
copyableTruncatedPoolId :: Double -> Color -> PoolID -> WidgetNode s AppEvent
copyableTruncatedPoolId fontSize mainColor (PoolID text) = 
  tooltip_ "Copy" [tooltipDelay 0] $ button (Text.take 10 text <> "...") (CopyText text)
    `styleBasic`
      [ padding 0
      , textLeft
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
