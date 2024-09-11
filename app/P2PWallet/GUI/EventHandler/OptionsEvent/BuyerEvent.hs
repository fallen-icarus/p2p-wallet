module P2PWallet.GUI.EventHandler.OptionsEvent.BuyerEvent
  ( 
    handleBuyerEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Prelude

handleBuyerEvent :: AppModel -> OptionsBuyerEvent -> [AppEventResponse AppModel AppEvent]
handleBuyerEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Set the new desired contract assets
  -----------------------------------------------
  SetNewContractAssets modal -> case modal of
    StartAdding _ ->
      let oldPair = optionsModel ^. #buyerModel % #selectedContractAssets
          newPairInfo = flip (maybe ("","","")) oldPair $ \(offerAsset,askAsset,mPremiumAsset) ->
            ( showAssetNameOnly reverseTickerMap $ toNativeAsset offerAsset
            , showAssetNameOnly reverseTickerMap $ toNativeAsset askAsset
            , maybe "" (showAssetNameOnly reverseTickerMap . toNativeAsset) mPremiumAsset
            )
       in [ Model $ model
              & #optionsModel % #buyerModel % #newContractAssets .~ newPairInfo
              & #optionsModel % #buyerModel % #choosingContractAssets .~ True
          ]
    CancelAdding ->
      [ Model $ model
          & #optionsModel % #buyerModel % #newContractAssets .~ ("","","")
          & #optionsModel % #buyerModel % #choosingContractAssets .~ False
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (OptionsEvent . OptionsBuyerEvent . SetNewContractAssets . AddResult) $ do
          let newAssets = optionsModel ^. #buyerModel % #newContractAssets
          fromRightOrAppError $ verifyContractAssets tickerMap newAssets
      ]
    AddResult targetAssets ->
      [ Model $ model 
          & #optionsModel % #buyerModel % #choosingContractAssets .~ False
          & #optionsModel % #buyerModel % #newContractAssets .~ ("","","")
          & #optionsModel % #buyerModel % #selectedContractAssets ?~ targetAssets
      , Task $ do
          -- Only sync the contracts for the new assets if it has not been synced yet.
          -- Users can manually resync if necessary.
          return $ case Map.lookup targetAssets $ optionsModel ^. #cachedProposals of
            Nothing -> OptionsEvent $ SyncOptionsProposals $ StartProcess $ Just targetAssets
            Just _ -> AppInit
      ]

  -----------------------------------------------
  -- Purchase options contract
  -----------------------------------------------
  PurchaseOptionsProposal modal -> case modal of
    StartProcess mTarget ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (OptionsEvent . OptionsBuyerEvent . PurchaseOptionsProposal . ProcessResults) $ do
          let Config{network} = config
          (idx,utxo) <- fromJustOrAppError "options purchase mTarget is Nothing" mTarget

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This options UTxO is already being spent.") $
            find (== utxo ^. #utxoRef) $
              map (view $ _2 % #proposalUTxO % #utxoRef) $ 
                txBuilderModel ^. #optionsBuilderModel % #proposalPurchases

          let newInput = optionsUTxOToProposalPurchase network idx utxo

          -- Check that all actions with the active beacons require the same redeemer.
          let newOptionsBuilderModel = 
                (txBuilderModel ^. #optionsBuilderModel) 
                  & #proposalPurchases %~ flip snoc (0,newInput)
          unless (hasOnlyOneOptionsActiveBeaconAction newOptionsBuilderModel) $
            throwIO $ AppError onlyOneOptionsActiveBeaconActionError

          -- There should be two results, the first is for the premium payment output and the
          -- second is for the new Active contract UTxO.
          minUTxOValues <- 
            calculateMinUTxOValue 
              network 
              (txBuilderModel ^? #parameters % _Just % _1) 
              -- Use a blank optionsBuilderModel to calculate the minUTxOValues for the purchase.
              (emptyOptionsBuilderModel & #proposalPurchases .~ [(0,newInput)])

          fromRightOrAppError $ updateOptionsPurchaseDeposits newInput minUTxOValues
      ]
    ProcessResults verifiedPurchase ->
      let keyNFT = mkNativeAsset Options.activeBeaconCurrencySymbol 
                 $ Options.unContractId 
                 $ Options.genContractId 
                 $ verifiedPurchase ^. #proposalUTxO % #utxoRef
       in [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              -- Add the new purchase with a dummy index.
              & #txBuilderModel % #optionsBuilderModel % #proposalPurchases %~ 
                  flip snoc (0,verifiedPurchase)
              -- Sort the purchases by the UTxO. This is required to generate 
              -- the outputs in the proper order.
              & #txBuilderModel % #optionsBuilderModel % #proposalPurchases %~ 
                  sortOn (view $ _2 % #proposalUTxO % #utxoRef)
              -- Reindex the proposals after sorting.
              & #txBuilderModel % #optionsBuilderModel % #proposalPurchases %~ reIndex
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "The new contract ID is: " <> display (keyNFT ^. #tokenName)
              , "The new Key NFT is: " <> display (keyNFT ^. #fingerprint)
              , ""
              , createOptionsPurchaseDepositMsg verifiedPurchase
              ]
          ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetAllProposalsFilters -> 
    [ Model $ model 
        & #optionsModel % #buyerModel % #proposalsFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

