module P2PWallet.GUI.Widgets.Delegation
  ( 
    delegationWidget
  ) where

import Monomer hiding (icon)
import Prettyprinter ((<+>), pretty, tupled)
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.StakeReward
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.StakeWallet
import P2PWallet.Data.Koios.DRep
import P2PWallet.Data.Koios.Pool
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Delegation.AddStakeWallet
import P2PWallet.GUI.Widgets.Delegation.PoolPicker
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Plutus
import P2PWallet.Prelude

delegationWidget :: AppModel -> AppNode
delegationWidget model@AppModel{..} = do
    zstack
      [ mainWidget model 
          `nodeVisible` and
            [ hasStakeWallets
            , not isAdding
            , not isEditing
            , not isDeleting
            ]
      , addFirstWalletWidget 
          `nodeVisible` and
            [ not isAdding 
            , not hasStakeWallets 
            , not isEditing 
            , not isDeleting
            ]
      , widgetIf isAdding $ addStakeWalletWidget model
      , widgetIf isEditing $ editStakeWalletWidget model
      , widgetIf isDeleting $ confirmDeleteWidget model
      , widgetIf (model ^. #delegationModel % #showPoolPicker) $ poolPickerWidget model -- picker
      , widgetIf (model ^. #delegationModel % #showPoolFilter) poolFilterWidget -- filter
      , widgetIf (isJust $ model ^. #delegationModel % #newDrepDelegation) $ getDRepWidget model
      ]
  where
    hasStakeWallets :: Bool
    hasStakeWallets = knownWallets ^. #stakeWallets /= []

    isAdding :: Bool
    isAdding = delegationModel ^. #addingWallet

    isEditing :: Bool
    isEditing = delegationModel ^. #editingWallet

    isDeleting :: Bool
    isDeleting = delegationModel ^. #deletingWallet

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
        ] `nodeVisible` not isAdding

-- The main widget that should only be shown if there are currently tracked stake wallets
-- AND no overlays need to be shown.
mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{..} =
    vscroll_ [wheelRate 50] $
      vstack
        [ spacer
        , centerWidgetH headerWidget
        , spacer
        , hgrid
            [ vstack
                [ box_ [alignMiddle] $ hstack
                    [ registrationStatusWidget registrationStatus
                    , spacer_ [width 5]
                    , totalDelegatedWidget totalDelegation
                    , spacer_ [width 5]
                    , rewardsBalanceWidget registrationStatus wallet
                    ]
                , spacer
                , delegationInfoWidget model
                ] `styleBasic` [padding 10]
            , vstack
                [ vstack 
                    [ box_ [alignMiddle] $ label "Reward History"
                        `styleBasic` [textFont "Italics", textSize 14, paddingT 10]
                    , separatorLine `styleBasic` [paddingL 70, paddingR 70, fgColor darkGray]
                    , widgetIf (rewardHistory /= []) $ historyTableWidget rewardHistory
                    , widgetIf (null rewardHistory) $ centerWidget $ 
                        flip styleBasic [padding 10, bgColor customGray4, radius 10] $ box $ 
                          label "This staking address has not earned any rewards yet."
                            `styleBasic` [textSize 8]
                    ] `styleBasic` 
                        [ bgColor customGray2, radius 15
                        , height 353
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
            , separatorLine `styleBasic` [paddingL 125, paddingR 125, fgColor darkGray]
            , spacer
            , widgetIf(null linkedAddresses) $ centerWidget $ 
                flip styleBasic [padding 10, bgColor customGray4, radius 10] $ box $ 
                  label "There are no active linked payment addresses."
                    `styleBasic` [textSize 12, textColor lightGray]
            , widgetIf(linkedAddresses /= []) $
                vscroll_ [scrollOverlay, wheelRate 50, barWidth 3, thumbWidth 3] $ 
                  vstack_ [childSpacing_ 1] $ 
                    for linkedAddresses $ \addr ->
                      centerWidgetH $ copyableLabelSelfWith 12 lightGray fitAddress (toText addr)
                        `styleBasic` [textCenter,radius 20, padding 5, bgColor customGray4]
            ] `styleBasic` [radius 15, bgColor customGray2, padding 10]
        ] `styleBasic` [padding 10]
  where
    wallet :: StakeWallet
    wallet@StakeWallet{..} = delegationModel ^. #selectedWallet

    stakeHash = fromRight (PubKeyCredential "") $ stakeAddressToPlutusCredential stakeAddress

    -- Shows an icon representing where the address is paired or watched.
    (walletTypeIcon,walletTypeTip)
      | isNothing stakeKeyDerivation = (watchedIcon,"Watched")
      | otherwise = (pairedIcon,"Paired")

    headerWidget :: AppNode
    headerWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
                `styleHover` [textSize 10, bgColor customGray2, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
                `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
      hstack
        [ hstack
            [ tooltip_ walletTypeTip [tooltipDelay 0] $ label walletTypeIcon
                `styleBasic` [textSize 12, textColor white, textMiddle, textFont "Remix"]
            , spacer_ [width 5]
            , vstack
                [ copyableLabelSelf 10 white $ display stakeAddress
                , hstack
                    [ copyableLabelSelf 8 lightGray $ display stakeHash
                    , widgetMaybe stakeKeyDerivation $ \keyInfo ->
                        hstack
                          [ spacer_ [width 5]
                          , flip styleBasic [textSize 10] $ 
                              tooltip_ (display keyInfo) [tooltipDelay 0] $
                                box_ [alignMiddle, onClick $ CopyText $ display keyInfo] $
                                  label remixRouteLine
                                    `styleBasic` 
                                      [ bgColor black
                                      , textMiddle
                                      , textFont "Remix"
                                      , textSize 8
                                      , textColor customBlue
                                      , paddingT 1
                                      , paddingB 1
                                      , paddingL 3
                                      , paddingR 3
                                      , radius 5
                                      ]
                                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                          ]
                    ]
                ]
            , spacer_ [width 1]
            , morePopup `styleBasic` [styleIf (isJust stakeKeyDerivation) $ paddingT 2]
            ] `styleBasic`
                [ bgColor customGray3
                , padding 10
                , radius 10
                , border 1 black
                ]
        , spacer
        , box_ [alignMiddle] $ textDropdown_ 
              (toLensVL $ #delegationModel % #selectedWallet) 
              (knownWallets ^. #stakeWallets) 
              (view #alias) 
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray3
              , width 110
              , paddingR 10
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 5]
        , box_ [alignMiddle] $ tooltip_ "Refresh" [tooltipDelay 0] $
            button refreshIcon (SyncWallets $ StartProcess Nothing)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 5
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              , textSize 12
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        ]

delegationInfoWidget :: AppModel -> AppNode
delegationInfoWidget model@AppModel{delegationModel=DelegationModel{..}} = do
    vstack
      [ hgrid_ [childSpacing_ 5]
          [ header "Stake Delegation" StakeDelegationScene
          , header "DRep Delegation" GovernanceDelegationScene
          ]
      , spacer_ [width 10]
      , stakeDelegationWidget model
          `nodeVisible` (scene == StakeDelegationScene)
      , voteDelegationWidget model
          `nodeVisible` (scene == GovernanceDelegationScene)
      ] `styleBasic` [bgColor customGray2, padding 10, radius 10, height 300]
  where
    header txt newScene =
      flip styleHover [bgColor customGray3, cursorIcon CursorHand, radius 5] $
        box_ [onClick $ DelegationEvent $ ChangeDelegationScene newScene] $
          vstack
            [ label txt
                `styleBasic` 
                  [ textSize 12
                  , configIf (scene == newScene) $ textColor customBlue
                  ]
            , spacer_ [width 5]
            , separatorLine
                `styleBasic`
                  [ configIf (scene == newScene) $ fgColor customBlue
                  , paddingL 10
                  , paddingR 10
                  ]
            ] `styleBasic` [padding 3, radius 3]

voteDelegationWidget :: AppModel -> AppNode
voteDelegationWidget AppModel{delegationModel=DelegationModel{..}} = do
  vstack
    [ widgetMaybe (selectedWallet ^. #delegatedDRep) drepInfoWidget
    , widgetIf (isNothing $ selectedWallet ^. #delegatedDRep) $ 
        voteNotDelegatedWidget (selectedWallet ^. #registrationStatus)
    ]

stakeDelegationWidget :: AppModel -> AppNode
stakeDelegationWidget AppModel{delegationModel=DelegationModel{..}} = do
  vstack
    [ widgetMaybe (selectedWallet ^. #delegatedPool) poolInfoWidget
    , widgetIf (isNothing $ selectedWallet ^. #delegatedPool) $ 
        stakeNotDelegatedWidget (selectedWallet ^. #registrationStatus)
    ]

voteNotDelegatedWidget :: RegistrationStatus -> AppNode
voteNotDelegatedWidget registrationStatus = do
  centerWidget $ vstack
    [ centerWidgetH $ label "Delegate to a DRep!"
        `styleBasic` [ textFont "Italics" ]
    , widgetIf (registrationStatus == NotRegistered) $
        vstack
          [ spacer_ [width 3]
          , centerWidgetH $ label (show $ tupled ["Don't forget to also register!"])
              `styleBasic` [ textColor customRed, textSize 12 ]
          ]
    , spacer
    , centerWidgetH $ mainButton "Delegate" $ 
        DelegationEvent $ AddDrepDelegation $ StartAdding $ Just ()
    ]

stakeNotDelegatedWidget :: RegistrationStatus -> AppNode
stakeNotDelegatedWidget registrationStatus = do
  box $ 
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

withdrawButton :: StakeWallet -> RegistrationStatus -> AppNode
withdrawButton stakeWallet registrationStatus = do
  let (tip,mainColor,highlightColor,event)
        | registrationStatus == Registered = 
            ( "Withdraw"
            , customBlue
            , customGray1
            , DelegationEvent $ AddSelectedUserWithdrawal stakeWallet)
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

registrationButton :: RegistrationStatus -> AppNode
registrationButton registrationStatus = do
  let (tip,icon,newAction)
        | registrationStatus == NotRegistered = ("Register",registerIcon,Registration)
        | otherwise = ("Deregister",deregisterIcon,Deregistration)
  tooltip_ tip [tooltipDelay 0] $ 
    box_ [onClick $ DelegationEvent $ AddSelectedUserCertificate (Nothing,newAction)] $ 
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

registrationStatusWidget :: RegistrationStatus -> AppNode
registrationStatusWidget registrationStatus = do
  vstack
    [ hstack
        [ label "Status"
            `styleBasic`
              [ textSize 8
              , textColor lightGray
              ]
        , registrationButton registrationStatus
        ]
    , spacer_ [width 3]
    , label (display registrationStatus)
        `styleBasic`
          [ textSize 9
          , if registrationStatus == NotRegistered then 
              textColor customRed 
            else 
              textColor customBlue
          ]
    ] `styleBasic`
        [ bgColor customGray2
        , padding 8
        , radius 8
        ]

totalDelegatedWidget :: Lovelace -> AppNode
totalDelegatedWidget totalDelegation = do
  vstack
    [ hstack
        [ label "Total Delegated"
            `styleBasic`
              [ textSize 8
              , textColor lightGray
              ]
        , mainTipButton totalDelegatedMsg
        ]
    , spacer_ [width 3]
    , label (display totalDelegation)
        `styleBasic`
          [ textSize 9
          ]
    ] `styleBasic`
        [ bgColor customGray2
        , padding 8
        , radius 8
        ]

rewardsBalanceWidget :: RegistrationStatus -> StakeWallet -> AppNode
rewardsBalanceWidget registrationStatus wallet@StakeWallet{availableRewards} = do
  vstack
    [ hstack
        [ label "Rewards Balance"
            `styleBasic`
              [ textSize 8
              , textColor lightGray
              ]
        , withdrawButton wallet registrationStatus
        ]
    , spacer_ [width 3]
    , label (display availableRewards)
        `styleBasic`
          [ textSize 9
          ]
    ] `styleBasic`
        [ bgColor customGray2
        , padding 8
        , radius 8
        ]

morePopup :: AppNode
morePopup = do
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
            , textSize 12
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

historyTableWidget :: [StakeReward] -> AppNode
historyTableWidget rewardHistory = do
  vstack
    [ hgrid_ [childSpacing_ 3]
        [ box_ [alignMiddle] $ hstack
            [ label "Epoch" `styleBasic` [textSize 10]
            , mainButton helpIcon (Alert spendableEpochMsg)
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , padding 2
                  , textSize 8
                  , textFont "Remix"
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ]
        , box_ [alignMiddle] $ label "Rewards" `styleBasic` [textSize 10]
        , box_ [alignMiddle] $ label "Pool ID" `styleBasic` [textSize 10]
        ] `styleBasic` [padding 5, bgColor customGray4, radiusTL 5, radiusTR 5]
    , spacer_ [width 1]
    , vscroll_ [scrollOverlay, wheelRate 50, thumbWidth 3] $ vstack_ [childSpacing_ 1] $ 
        for (take 50 rewardHistory) $ \StakeReward{..} ->
          hgrid_ [childSpacing_ 3]
            [ box_ [alignMiddle] $ label (show spendableEpoch) 
                `styleBasic` [padding 0, textSize 9, textColor lightGray]
            , box_ [alignMiddle] $ label (display amount) 
                `styleBasic` [padding 0, textSize 9, textColor lightGray]
            , box_ [alignMiddle] $ copyableTruncatedPoolId 9 lightGray poolId
            ] `styleBasic` [padding 5, bgColor customGray4]
    ] `styleBasic` [ padding 10 ]

poolInfoWidget :: Pool -> AppNode
poolInfoWidget Pool{..} = do
    let PoolInfo{..} = fromMaybe def info
        nameAndTicker = show $ pretty name <+> tupled [pretty ticker]
    vstack
      [ hstack
          [ label nameAndTicker `styleBasic` [textSize 12]
          , filler
          , changeDelegationButton
          ]
      , spacer_ [width 5]
      , box_ [alignLeft] $ copyableLabelSelfWith 9 lightGray fitId $ display poolId
      , spacer_ [width 3]
      , box_ [alignLeft] $ copyableLabelSelf 9 lightGray homepage
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
      , subField "Pledge" pledgeMsg $ display $ fromMaybe 0 pledge
      , spacer
      , subField "Active Pledge" activePledgeMsg $ display $ fromMaybe 0 livePledge
      , spacer
      , subField "Cost" fixedCostMsg $ display $ fromMaybe 0 fixedCost
      ] 
  where
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

drepInfoWidget :: DRep -> AppNode
drepInfoWidget DRep{..} =
    vstack
      [ hstack
          [ vstack
              [ box_ [alignLeft] $ copyableLabelSelfWith 9 lightGray fitId $ display drepId
              , spacer_ [width 3]
              , flip (maybe $ label "No Website" `styleBasic` [textSize 9, textColor lightGray]) url $ 
                  \u -> box_ [alignLeft] $ copyableLabelSelf 9 lightGray u
              ]
          , filler
          , box_ [alignTop] changeDelegationButton
          ]
      , spacer
      , widgetMaybe expiresEpoch $ \epoch ->
          vstack
            [ label ("WARNING: This DRep is expiring on epoch " <> show epoch)
                `styleBasic`
                    [textFont "Italics", textSize 12, textColor customRed]
            , spacer
            ]
      , subField "Total DRep Voting Power" votingPowerMsg $ display amount
      ] 
  where
    changeDelegationButton :: AppNode
    changeDelegationButton = do
      tooltip_ "Change Delegation" [tooltipDelay 0] $ 
        box_ [onClick $ DelegationEvent $ AddDrepDelegation $ StartAdding $ Just ()] $ 
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

editStakeWalletWidget :: AppModel -> AppNode
editStakeWalletWidget _ = do
  centerWidget $ vstack 
    [ vstack_ [childSpacing]
        [ hstack 
            [ label "Wallet Name:"
            , spacer
            , textField (toLensVL $ #delegationModel % #newAliasField) 
                `styleBasic` [width 300, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DelegationEvent $ ChangeStakeWalletName CancelAdding
        , spacer
        , mainButton "Confirm" $ DelegationEvent $ ChangeStakeWalletName ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

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
    ] `styleBasic` [bgColor customGray3, padding 20]

getDRepWidget :: AppModel -> AppNode
getDRepWidget model = do
  let rootLens = #delegationModel % #newDrepDelegation
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ bgColor customBlue ]
      choiceButton caption field targetLens =
        optionButton_ caption field targetLens
          [optionButtonOffStyle offStyle]
          `styleBasic` 
            [ bgColor customBlue
            , border 0 transparent
            , textColor white
            , radius 5
            , textSize 12
            ]
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ 
            label "Vote Delegation"
              `styleBasic` [textColor customBlue, textFont "Italics"]
        , spacer
        , centerWidgetH $ hstack_ [childSpacing]
            [ label "Delegate To:"
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "DRep" (Just $ DRepDelegation "" False) (toLensVL rootLens)
                , choiceButton "Always Abstain" (Just AlwaysAbstainDelegation) (toLensVL rootLens)
                , choiceButton "Always No" (Just AlwaysNoDelegation) (toLensVL rootLens)
                ]
            ]
        , spacer
        , centerWidgetH $ hstack
            [ label "DRep ID:"
                `styleBasic` [textSize 12]
            , spacer
            , textField (toLensVL $ #delegationModel % #newDrepId) 
                `styleBasic` [textSize 8, width 300, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 2]
            , box_ [onClick $ Alert drepIdMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , padding 5
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ] `nodeVisible` (model ^. rootLens == Just (DRepDelegation "" False))
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ DelegationEvent $ AddDrepDelegation CancelAdding
            , spacer
            , mainButton "Confirm" $ DelegationEvent $ AddDrepDelegation ConfirmAdding
            ]
        ] `styleBasic` [bgColor customGray3, padding 5, paddingL 20, paddingR 20, radius 10]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        ]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
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

subField :: Text -> Text -> Text -> AppNode
subField caption helpMsg field =
  vstack
    [ hstack
        [ label caption
            `styleBasic`
              [ textSize 8
              , textColor lightGray
              ]
        , subTipButton helpMsg
        ]
    , spacer_ [width 3]
    , label field
        `styleBasic`
          [ textSize 10
          ]
    ] `styleBasic`
        [ bgColor customGray4
        , padding 5
        , radius 5
        ]

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

-- | A label button that will copy itself but show a formatted version.
copyableLabelSelfWith :: Double -> Color -> (Text -> Text) -> Text -> WidgetNode s AppEvent
copyableLabelSelfWith fontSize mainColor modifier fullInfo = do
  let formattedInfo = modifier fullInfo
  tooltip_ "Copy" [tooltipDelay 0] $ button formattedInfo (CopyText fullInfo)
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

fitId :: Text -> Text
fitId tId = Text.take 20 tId <> "..." <> Text.drop 40 tId

fitAddress :: Text -> Text
fitAddress address
  | Text.length address > 80 = Text.take 40 address <> "..." <> Text.drop 80 address
  | otherwise = address
