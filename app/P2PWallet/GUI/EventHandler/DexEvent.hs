module P2PWallet.GUI.EventHandler.DexEvent
  ( 
    handleDexEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

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
  -- Add new swap wallet
  -----------------------------------------------
  AddNewDexWallet modal -> case modal of
    StartAdding _ -> 
      let currentDexWalletIds = map (view #stakeId) $ knownWallets ^. #dexWallets
          availWallets = filter ((`notElem` currentDexWalletIds) . view #stakeId) $ 
            knownWallets ^. #stakeWallets
      in  [ Model $ model 
              & #dexModel % #addingWallet .~ True -- Show widget.
              & #dexModel % #newSwapCredential .~ maybeHead availWallets
          ]
    CancelAdding -> 
      [ Model $ model 
          & #dexModel % #addingWallet .~ False -- Show widget.
          & #dexModel % #newSwapCredential .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (DexEvent . AddNewDexWallet . AddResult) $ do
          let stakeWallet = fromMaybe def $ dexModel ^. #newSwapCredential

          -- Get the new payment id for the new entry into the swap_wallet table.
          paymentId <- getNextPaymentIdAcrossTables databaseFile >>= fromRightOrAppError

          -- Create the new Dex wallet.
          verifiedDexWallet <- 
            fromRightOrAppError $ processNewDexWallet paymentId stakeWallet
          
          -- Add the new payment wallet to the database.
          insertDexWallet databaseFile verifiedDexWallet >>= fromRightOrAppError

          return verifiedDexWallet
      ]
    AddResult verifiedDexWallet ->
      [ Model $ model 
          & #knownWallets % #dexWallets %~ flip snoc verifiedDexWallet
          & #dexModel % #addingWallet .~ False
          & #dexModel % #selectedWallet .~ verifiedDexWallet
      , Task $ return $ SyncWallets StartProcess
      ]

  -----------------------------------------------
  -- Delete Swap Wallet
  -----------------------------------------------
  DeleteDexWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #dexModel % #deletingWallet .~ True
                      & #dexModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #dexModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ DexEvent $ DeleteDexWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = dexModel ^. #selectedWallet % #paymentId

          -- Delete the payment wallet.
          deleteDexWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the payment wallet from the cached list of wallets.
      let currentId = dexModel ^. #selectedWallet % #paymentId
          newWallets = filter (\w -> w ^. #paymentId /= currentId) $
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
      [ Model $ model 
          & #dexModel % #choosingTradingPair .~ False
          & #dexModel % #newTradingPair .~ ("","")
          & #dexModel % #selectedTradingPair ?~ tradingPair
          -- Configure the new swap creation info to use the new assets.
          & #dexModel % #newSwapCreation .~ createNewSwapCreation (config ^. #network) tradingPair
      , Task $ do
          -- Only sync the order book for the new trading pair if it has not been synced yet.
          -- Users can manually resync if necessary.
          case Map.lookup tradingPair $ dexModel ^. #cachedOrderBooks of
            Nothing -> return $ DexEvent $ SyncOrderBook StartProcess
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
    StartProcess ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddNewLimitOrderCreation . ProcessResults) $ do
          let swapAddress = dexModel ^. #selectedWallet % #oneWaySwapAddress
          verifiedSwap <- fromRightOrAppError $ processNewSwapCreation swapAddress reverseTickerMap $
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
                (txBuilderModel ^. #parameters) 
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
    StartProcess ->
      [ Model $ model & #waitingStatus % #addingToBuilder .~ True
      , Task $ runActionOrAlert (DexEvent . AddNewLimitOrderCreation . ProcessResults) $ do
          let swapAddress = dexModel ^. #selectedWallet % #twoWaySwapAddress
          verifiedSwap <- fromRightOrAppError $ processNewSwapCreation swapAddress reverseTickerMap $
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
                (txBuilderModel ^. #parameters) 
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
    StartProcess ->
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
    let DexWallet{network,alias,stakeAddress,stakeKeyPath} = dexModel ^. #selectedWallet
        newInput = swapUTxOToSwapClose network alias stakeAddress stakeKeyPath swapUTxO
    in  case processNewSwapClose newInput txBuilderModel of
          Left err -> [ Task $ return $ Alert err ]
          Right newTxModel ->
            [ Model $ model & #txBuilderModel .~ newTxModel
            , Task $ return $ Alert "Successfully added to builder!"
            ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Process the new stake credential to generate the dex wallet info. Generete the one-way
-- and two-way swap addresses for the desired stake credential.
processNewDexWallet :: PaymentId -> StakeWallet -> Either Text DexWallet
processNewDexWallet paymentId StakeWallet{..} = do
  stakeCred <- stakeAddressToPlutusCredential stakeAddress
  oneWaySwapAddress <- OneWay.genSwapAddress network $ Just stakeCred
  twoWaySwapAddress <- TwoWay.genSwapAddress network $ Just stakeCred
  return $ DexWallet
    { network = network
    , profileId = profileId
    , paymentId = paymentId
    , stakeId = stakeId
    , alias = alias
    , oneWaySwapAddress = oneWaySwapAddress
    , twoWaySwapAddress = twoWaySwapAddress
    , stakeAddress = stakeAddress
    , stakeKeyPath = stakeKeyPath
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
    find (\i -> i ^. _2 % #utxoRef == utxoRef) swapCloses

  -- Get the input's new index.
  let newIdx = length swapCloses

  -- Add the new close to the end of the list of swap closes.
  return $ balanceTx $ model & #swapBuilderModel % #swapCloses %~ flip snoc (newIdx,u)
