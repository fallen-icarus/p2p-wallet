module P2PWallet.GUI.EventHandler.DexEvent
  ( 
    handleDexEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.BalanceTx
import P2PWallet.Actions.CalculateMinUTxOValue
import P2PWallet.Actions.Database
import P2PWallet.Actions.SyncOrderBooks
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Prelude

handleDexEvent :: AppModel -> DexEvent -> [AppEventResponse AppModel AppEvent]
handleDexEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeDexScene newScene -> 
    [ Model $ model & #dexModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowDexMorePopup -> 
    [ Model $ model & #dexModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Clear limit order offer quantity
  -----------------------------------------------
  ClearLimitOrderFields ->
    [ Model $ model 
        & #dexModel % #newSwapCreation % #offerQuantity .~ "" 
        & #dexModel % #newSwapCreation % #askPerOfferPrice .~ "" 
        -- The arbitrageFee fee can be left alone since it doesn't really depend on the swap
        -- direction.
    ]
    
  -----------------------------------------------
  -- Clear the `NewSwapCreation` form
  -----------------------------------------------
  ClearNewSwapForm ->
    [ Model $ model & #dexModel % #newSwapCreation %~ clearNewSwapCreation ]
    
  -----------------------------------------------
  -- Add new dex wallet
  -----------------------------------------------
  AddNewDexWallet modal -> case modal of
    StartAdding _ -> 
      let currentDexWalletIds = map (view #stakeWalletId) $ knownWallets ^. #dexWallets
          availWallets = filter ((`notElem` currentDexWalletIds) . view #stakeWalletId) $ 
            knownWallets ^. #stakeWallets
      in  [ Model $ model 
              & #dexModel % #addingWallet .~ True -- Show widget.
              & #dexModel % #targetStakeCredential .~ maybeHead availWallets
          ]
    CancelAdding -> 
      [ Model $ model 
          & #dexModel % #addingWallet .~ False -- Show widget.
          & #dexModel % #targetStakeCredential .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (DexEvent . AddNewDexWallet . AddResult) $ do
          stakeWallet <- fromJustOrAppError "targetStakeCredential is Nothing" $ 
            dexModel ^. #targetStakeCredential

          -- Get the new wallet id for the new entry into the dex_wallet table.
          dexWalletId <- getNextDexWalletId databaseFile >>= fromRightOrAppError

          -- Create the new Dex wallet.
          verifiedDexWallet <- 
            fromRightOrAppError $ processNewDexWallet dexWalletId stakeWallet
          
          -- Add the new payment wallet to the database.
          insertDexWallet databaseFile verifiedDexWallet >>= fromRightOrAppError

          return verifiedDexWallet
      ]
    AddResult verifiedDexWallet ->
      [ Model $ model 
          & #knownWallets % #dexWallets %~ flip snoc verifiedDexWallet
          & #dexModel % #addingWallet .~ False
          & #dexModel % #selectedWallet .~ verifiedDexWallet
      , Task $ return $ SyncWallets $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Delete Dex Wallet
  -----------------------------------------------
  DeleteDexWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model 
          & #dexModel % #deletingWallet .~ True
          & #dexModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #dexModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ DexEvent $ DeleteDexWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = dexModel ^. #selectedWallet % #dexWalletId

          -- Delete the payment wallet.
          deleteDexWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the payment wallet from the cached list of wallets.
      let currentId = dexModel ^. #selectedWallet % #dexWalletId
          newWallets = filter (\w -> w ^. #dexWalletId /= currentId) $
            knownWallets ^. #dexWallets
      in [ Model $ model 
            & #dexModel % #deletingWallet .~ False
            & #knownWallets % #dexWallets .~ newWallets
            & #dexModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
         ]

  -----------------------------------------------
  -- Set the new desired trading pair
  -----------------------------------------------
  SetNewTradingPair modal -> case modal of
    StartAdding _ ->
      let oldPair = dexModel ^. #selectedTradingPair
          newPairInfo = flip (maybe ("","")) oldPair $ \(offerAsset,askAsset) ->
            ( showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
            , showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
            )
      in  [ Model $ model
              & #dexModel % #newTradingPair .~ newPairInfo
              & #dexModel % #choosingTradingPair .~ True
          ]
    CancelAdding ->
      [ Model $ model
          & #dexModel % #newTradingPair .~ ("","")
          & #dexModel % #choosingTradingPair .~ False
      ]
    ConfirmAdding ->
      [ Task $ runActionOrAlert (DexEvent . SetNewTradingPair . AddResult) $ do
          let (newOffer,newAsk) = dexModel ^. #newTradingPair
          offerAsset <- 
            OfferAsset <$> fromRightOrAppError (parseNativeAssetName tickerMap newOffer)
          askAsset <- 
            AskAsset <$> fromRightOrAppError (parseNativeAssetName tickerMap newAsk)

          when (unOfferAsset offerAsset == unAskAsset askAsset) $
            throwIO $ AppError "The offer and ask assets are the same asset."

          return (offerAsset, askAsset)
      ]
    AddResult tradingPair ->
      let DexWallet{alias,network} = dexModel ^. #selectedWallet in
      [ Model $ model 
          & #dexModel % #choosingTradingPair .~ False
          & #dexModel % #newTradingPair .~ ("","")
          & #dexModel % #selectedTradingPair ?~ tradingPair
          -- Configure the new swap creation info to use the new assets.
          & #dexModel % #newSwapCreation .~ createNewSwapCreation network alias tradingPair
      , Task $ do
          -- Only sync the order book for the new trading pair if it has not been synced yet.
          -- Users can manually resync if necessary.
          case Map.lookup tradingPair $ dexModel ^. #cachedOrderBooks of
            Nothing -> return $ DexEvent $ SyncOrderBook $ StartProcess Nothing
            Just _ -> return AppInit
      ]

  -----------------------------------------------
  -- Invert the order book
  -----------------------------------------------
  InvertOrderBook -> 
    let (OfferAsset offerAsset,AskAsset askAsset) = fromMaybe def $ dexModel ^. #selectedTradingPair in
    [ Event $ DexEvent $ SetNewTradingPair $ AddResult (OfferAsset askAsset, AskAsset offerAsset) ]

  -----------------------------------------------
  -- Create New Limit Order
  -----------------------------------------------
  AddNewLimitOrderCreation modal -> case modal of
    StartProcess _ ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddNewLimitOrderCreation . ProcessResults) $ do
          let swapAddress = dexModel ^. #selectedWallet % #oneWaySwapAddress
          verifiedSwap <- fromRightOrAppError $ verifyNewSwapCreation swapAddress reverseTickerMap $
            -- Remove any entries that are not needed for the limit order.
            (dexModel ^. #newSwapCreation)
              & #askQuantity .~ Nothing
              & #offerPerAskPrice .~ Nothing
              & #swapType .~ LimitOrder -- Label the kind of swap.

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank swapBuilderModel to calculate the minUTxOValue for the new swap.
                (emptySwapBuilderModel & #swapCreations .~ [(0,verifiedSwap)])

          -- Return the `SwapCreation` with the updated deposit field.
          return $ verifiedSwap & #deposit .~ minUTxOValue
      ]
    ProcessResults verifiedSwapCreation ->
      -- Get the index for the new swap creation.
      let newIdx = length $ txBuilderModel ^. #swapBuilderModel % #swapCreations 
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #swapBuilderModel % #swapCreations %~ 
                  flip snoc (newIdx,verifiedSwapCreation)
              & #dexModel % #newSwapCreation %~ clearNewSwapCreation
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This swap requires a deposit of: " <> display (verifiedSwapCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Create New Liquidity Swap
  -----------------------------------------------
  AddNewLiquiditySwapCreation modal -> case modal of
    StartProcess _ ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddNewLimitOrderCreation . ProcessResults) $ do
          let swapAddress = dexModel ^. #selectedWallet % #twoWaySwapAddress
          verifiedSwap <- fromRightOrAppError $ verifyNewSwapCreation swapAddress reverseTickerMap $
            -- Remove any entries that are not needed for the liquidity swap.
            (dexModel ^. #newSwapCreation)
              & #arbitrageFee .~ "0.0"
              & #tradingPairInverted .~ False
              & #swapType .~ LiquiditySwap -- Label the swap type.

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <-
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                (config ^. #network) 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank swapBuilderModel to calculate the minUTxOValue for the new swap.
                (emptySwapBuilderModel & #swapCreations .~ [(0,verifiedSwap)])

          -- Return the `SwapCreation` with the updated deposit field.
          return $ verifiedSwap & #deposit .~ minUTxOValue
      ]
    ProcessResults verifiedSwapCreation ->
      -- Get the index for the new swap creation.
      let newIdx = length $ txBuilderModel ^. #swapBuilderModel % #swapCreations 
      in  [ Model $ model 
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #swapBuilderModel % #swapCreations %~ 
                  flip snoc (newIdx,verifiedSwapCreation)
              -- Reset the `NewSwapCreation` form.
              & #dexModel % #newSwapCreation %~ clearNewSwapCreation
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This swap requires a deposit of: " <> display (verifiedSwapCreation ^. #deposit)
              ]
          ]

  -----------------------------------------------
  -- Sync Order Book
  -----------------------------------------------
  SyncOrderBook modal -> case modal of
    StartProcess _ ->
      [ Model $ model & #waitingStatus % #syncingOrderBook .~ True
      , Task $ do
          let (offerAsset,askAsset) = fromMaybe def $ dexModel ^. #selectedTradingPair
          runActionOrAlert (DexEvent . SyncOrderBook . ProcessResults) $ 
            syncOrderBooks (config ^. #network) offerAsset askAsset $ dexModel ^. #cachedOrderBooks
      ]
    ProcessResults newCachedOrderBooks ->
      [ Model $ model 
          & #waitingStatus % #syncingOrderBook .~ False
          & #dexModel % #cachedOrderBooks .~ newCachedOrderBooks
      ]

  -----------------------------------------------
  -- Add Swap Close to Builder
  -----------------------------------------------
  AddSelectedSwapClose swapUTxO ->
    let DexWallet{network,alias,stakeAddress,stakeKeyDerivation} = dexModel ^. #selectedWallet
        newInput = swapUTxOToSwapClose network alias stakeAddress stakeKeyDerivation swapUTxO
    in  case processNewSwapClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

  -----------------------------------------------
  -- Add Selected Swap Update to Builder
  -----------------------------------------------
  AddSelectedSwapUpdate modal -> case modal of
    StartAdding mTarget ->
      let DexWallet{network,alias} = dexModel ^. #selectedWallet
          newSwapUpdate = 
            (,) <$> mTarget
                <*> (swapUTxOToNewSwapCreation network alias reverseTickerMap <$> mTarget)
      in  [ Model $ model
              & #dexModel % #newSwapUpdate .~ newSwapUpdate
          ]
    CancelAdding ->
      [ Model $ model
          & #dexModel % #newSwapUpdate .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddSelectedSwapUpdate . AddResult) $ do
          let DexWallet{network,alias,stakeAddress,stakeKeyDerivation} = dexModel ^. #selectedWallet
          (utxoToClose@SwapUTxO{utxoRef},newSwap@NewSwapCreation{paymentAddress}) <- 
            fromJustOrAppError "Nothing set for `newSwapUpdate`" $ dexModel ^. #newSwapUpdate

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This swap is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $ 
                  txBuilderModel ^. #swapBuilderModel % #swapCloses
              , map (view $ _2 % #oldSwap % #utxoRef) $ 
                  txBuilderModel ^. #swapBuilderModel % #swapUpdates
              , map (view $ _2 % #utxoRef) $
                  txBuilderModel ^. #swapBuilderModel % #swapExecutions
              ])

          verifiedSwapUpdate <- fromRightOrAppError $ 
            verifyNewSwapCreation paymentAddress reverseTickerMap newSwap

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                network 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank swapBuilderModel to calculate the minUTxOValue for the new swap.
                -- This just uses `swapCreations` because the output only depends on the creation
                -- part.
                (emptySwapBuilderModel & #swapCreations .~ [(0,verifiedSwapUpdate)])

          return $ SwapUpdate
            { oldSwap = swapUTxOToSwapClose network alias stakeAddress stakeKeyDerivation utxoToClose
            , newSwap = verifiedSwapUpdate & #deposit .~ minUTxOValue
            }
      ]
    AddResult verifiedSwapUpdate ->
      -- Get the index for the new swap creation.
      let newIdx = length $ txBuilderModel ^. #swapBuilderModel % #swapUpdates
      in  [ Model $ model
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #swapBuilderModel % #swapUpdates %~ 
                  flip snoc (newIdx,verifiedSwapUpdate)
              & #dexModel % #newSwapUpdate .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert $ unlines
              [ "Successfully added to builder!"
              , ""
              , "This swap requires a deposit of: " <> 
                  display (verifiedSwapUpdate ^. #newSwap % #deposit)
              ]
          ]

  -----------------------------------------------
  -- Add Selected Swap Execution to Builder
  -----------------------------------------------
  AddSelectedSwapExecution modal -> case modal of
    StartAdding mTarget ->
      let (offerAsset,askAsset,swapUTxO) = fromMaybe def mTarget
          newSwapExecution = 
            swapUTxOToNewSwapExecution 
              (config ^. #network) 
              offerAsset 
              askAsset 
              reverseTickerMap 
              swapUTxO
      in  [ Model $ model
              & #dexModel % #newSwapExecution ?~ newSwapExecution
          ]
    CancelAdding ->
      [ Model $ model
          & #dexModel % #newSwapExecution .~ Nothing
      ]
    ConfirmAdding ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddSelectedSwapExecution . AddResult) $ do
          newSwap@NewSwapExecution{utxoRef,network} <- 
            fromJustOrAppError "Nothing set for `newSwapExecution`" $ dexModel ^. #newSwapExecution

          -- Verify that the new utxo is not already being spent.
          flip whenJust (const $ throwIO $ AppError "This swap is already being spent.") $
            find (== utxoRef) (concat
              [ map (view $ _2 % #utxoRef) $ 
                  txBuilderModel ^. #swapBuilderModel % #swapCloses
              , map (view $ _2 % #oldSwap % #utxoRef) $ 
                  txBuilderModel ^. #swapBuilderModel % #swapUpdates
              , map (view $ _2 % #utxoRef) $
                  txBuilderModel ^. #swapBuilderModel % #swapExecutions
              ])

          verifiedSwapExecution <- fromRightOrAppError $ 
            verifyNewSwapExecution reverseTickerMap newSwap

          -- There should only be one output in the `TxBody` for this action.
          minUTxOValue <- 
            fromJustOrAppError "`calculateMinUTxOValue` did not return results" . maybeHead =<<
              calculateMinUTxOValue 
                network 
                (txBuilderModel ^? #parameters % _Just % _1) 
                -- Use a blank swapBuilderModel to calculate the minUTxOValue for the new swap.
                (emptySwapBuilderModel & #swapExecutions .~ [(0,verifiedSwapExecution)])

          -- Check if the swap output contains enough ADA. Also account for whether ada is the 
          -- part of the trading pair.
          fromRightOrAppError $ updateMinUTxO verifiedSwapExecution minUTxOValue
      ]
    AddResult verifiedSwapExecution@SwapExecution{lovelace,deposit} ->
      -- Get the index for the new swap creation.
      let newIdx = length $ txBuilderModel ^. #swapBuilderModel % #swapExecutions
          successMsg
            | lovelace >= deposit = "Successfully added to builder!"
            | otherwise = unlines
                [ "Successfully added to builder!"
                , ""
                , "The swap minUTxOValue increased by: " <> display (deposit - lovelace)
                , "You will need to cover the increase to execute this swap."
                ]
      in  [ Model $ model
              & #waitingStatus % #addingToBuilder .~ False
              & #txBuilderModel % #swapBuilderModel % #swapExecutions %~ 
                  flip snoc (newIdx,verifiedSwapExecution)
              & #dexModel % #newSwapExecution .~ Nothing
              & #txBuilderModel %~ balanceTx
          , Task $ return $ Alert successMsg
          ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetPositionsFilters -> 
    [ Model $ model 
        & #dexModel % #positionsFilterModel .~ def
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]
  ResetDexTxFilters -> 
    let newDefault = def & #dateRange % _1 ?~ addDays (-30) (config ^. #currentDay) in
    [ Model $ model 
        & #dexModel % #txFilterModel .~ newDefault 
        & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

  -----------------------------------------------
  -- Inspecting Transactions
  -----------------------------------------------
  InspectDexTransaction tx -> 
    [ Model $ model & #dexModel % #inspectedTransaction ?~ tx ]
  CloseInspectedDexTransaction -> 
    [ Model $ model & #dexModel % #inspectedTransaction .~ Nothing ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Process the new stake credential to generate the dex wallet info. Generete the one-way
-- and two-way swap addresses for the desired stake credential.
processNewDexWallet :: DexWalletId -> StakeWallet -> Either Text DexWallet
processNewDexWallet dexWalletId StakeWallet{..} = do
  stakeCred <- stakeAddressToPlutusCredential stakeAddress
  oneWaySwapAddress <- OneWay.genSwapAddress network $ Just stakeCred
  twoWaySwapAddress <- TwoWay.genSwapAddress network $ Just stakeCred
  return $ DexWallet
    { network = network
    , profileId = profileId
    , dexWalletId = dexWalletId
    , stakeWalletId = stakeWalletId
    , alias = alias
    , oneWaySwapAddress = oneWaySwapAddress
    , twoWaySwapAddress = twoWaySwapAddress
    , stakeAddress = stakeAddress
    , stakeKeyDerivation = stakeKeyDerivation
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }

-- | Validate the new swap close and add it to the builder. Balance the transaction after.
processNewSwapClose :: SwapClose -> TxBuilderModel -> Either Text TxBuilderModel
processNewSwapClose u@SwapClose{utxoRef} model@TxBuilderModel{swapBuilderModel=SwapBuilderModel{..}} = do
  -- Verify that the new utxo is not already being spent.
  maybeToLeft () $ "This swap is already being spent." <$
    find (== utxoRef) (concat
      [ map (view $ _2 % #utxoRef) swapCloses
      , map (view $ _2 % #oldSwap % #utxoRef) swapUpdates
      , map (view $ _2 % #utxoRef) swapExecutions
      ])

  -- Get the input's new index.
  let newIdx = length swapCloses

  -- Add the new close to the end of the list of swap closes.
  return $ balanceTx $ model & #swapBuilderModel % #swapCloses %~ flip snoc (newIdx,u)
