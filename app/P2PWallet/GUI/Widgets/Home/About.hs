{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.About where

import Monomer
import Monomer.Lens qualified as L
import Data.Maybe (fromJust)
import Prettyprinter (pretty)

import P2PWallet.Data.App
import P2PWallet.Data.Core
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

aboutWidget :: AppWenv -> AppModel -> AppNode
aboutWidget wenv model = do
  vstack 
    [ hstack [spacer, addressInfoWidget wenv model, spacer]
    , filler
    , hstack
        [ filler
        , box (mainButton "Pair" $ HomeEvent $ PairPaymentWallet StartAdding) 
            `styleBasic` [paddingR 5, paddingB 20, paddingL 20, paddingT 20]
        , box (mainButton "Watch" $ HomeEvent $ WatchPaymentWallet StartAdding) 
            `styleBasic` [paddingL 5, paddingB 20, paddingR 20, paddingT 20]
        ]
    ] `nodeVisible` (not (model ^. homeModel . pairing) && not (model ^. homeModel . watching))

-- | Show key information about the address.
addressInfoWidget :: AppWenv -> AppModel -> AppNode
addressInfoWidget wenv model = do
  let wallet = model ^. homeModel . selectedWallet
      eInfo = inspectBech32Address $ unPaymentAddress $ wallet ^. paymentAddress
      spendingKeyHash = either (const $ Just "") infoSpendingKeyHash eInfo
      stakeKeyHash = either (const Nothing) infoStakeKeyHash eInfo
      stakeAddr = wallet ^. stakeAddress
      stakeWallet = fromMaybe def $
        find (\w -> stakeAddr == Just (w ^. stakeAddress)) $ model ^. wallets . stakeWallets
      delegationStatus
        | stakeWallet == def = "This address does not support staking"
        | stakeWallet ^. registrationStatus == NotRegistered = "Not Registered"
        | otherwise = maybe "Not Delegated" (toText . view poolId) $ stakeWallet ^. delegatedPool
      sectionBgColor = wenv ^. L.theme . L.sectionColor

  vstack 
    [ centerWidgetH $ label "Address Info" 
        `styleBasic` [textFont "Italics", textSize 18, padding 10]
    , vstack
        [ hstack
            [ spacer
            , spacer
            , label "Payment Credential" `styleBasic` [textFont "Italics"]
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Payment Address:"
            , spacer
            , copyableTextArea $ toText $ wallet ^. paymentAddress
            ] `styleBasic` [padding 5]
        , hstack 
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Payment Key Hash:" 
            , spacer
            , flip styleBasic [width 550] $ copyableTextArea $ 
                show $ PubKeyHash $ BuiltinByteString $ fromJust spendingKeyHash
            ] `styleBasic` [padding 5]
        , hstack
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Derivation Path:"
            , spacer
            , copyableTextArea $ fromMaybe "none" $
                show . pretty . showDerivationPath <$> wallet ^. paymentKeyPath
            ] `styleBasic` [padding 5]
              `nodeVisible` (isJust $ wallet ^. paymentKeyPath)
        ]
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
            , copyableTextArea $ maybe "none" toText $ wallet ^. stakeAddress
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
              `nodeVisible` (isJust $ wallet ^. stakeAddress)
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
              `nodeVisible` (isJust $ wallet ^. stakeAddress)
        , hstack
            [ spacer
            , spacer
            , spacer
            , spacer
            , label "Delegation Status:"
            , spacer
            , copyableTextArea delegationStatus
            ] `styleBasic` [padding 5]
              `nodeVisible` (isJust $ wallet ^. stakeAddress)
        ]
    , spacer
    ] `styleBasic` [radius 5, bgColor sectionBgColor]
