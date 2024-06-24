module P2PWallet.GUI.EventHandler.TickerRegistryEvent
  ( 
    handleTickerRegistryEvent
  ) where

import Monomer hiding (decimals)
import Data.Map qualified as Map

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Prelude

handleTickerRegistryEvent 
  :: AppModel 
  -> TickerRegistryEvent 
  -> [AppEventResponse AppModel AppEvent]
handleTickerRegistryEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Add new ticker
  -----------------------------------------------
  AddNewTickerInfo modal -> case modal of
    StartAdding ticker -> 
      [ Model $ model & #tickerRegistryModel % #addingTicker .~ True -- Show widget.
                      & #tickerRegistryModel % #newTickerInfo % #ticker .~ fromMaybe "" ticker
                      & #tickerRegistryModel % #newTickerInfo % #policyId .~ ""
                      & #tickerRegistryModel % #newTickerInfo % #assetName .~ ""
                      & #tickerRegistryModel % #newTickerInfo % #decimals .~ 0
      ]
    CancelAdding -> 
      [ Model $ model & #tickerRegistryModel % #addingTicker .~ False
                      & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TickerRegistryEvent . AddNewTickerInfo . AddResult) $ do
          let tickerInfo = tickerRegistryModel ^. #newTickerInfo

          -- Validate the new ticker info.
          verifiedTickerInfo <- fromRightOrAppError $ 
            processNewTickerInfo tickerInfo reverseTickerMap

          -- Add the new ticker to the database.
          insertTickerInfo databaseFile verifiedTickerInfo >>= fromRightOrAppError

          return verifiedTickerInfo
      ]
    AddResult TickerInfo{..} ->
      [ Model $
          model & #tickerRegistryModel % #addingTicker .~ False
                & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
                & #tickerMap %~ Map.insert ticker (policyId,assetName,decimals)
                & #reverseTickerMap %~ 
                    Map.insert (policyId,assetName) (ticker,decimals)
      ]

  -----------------------------------------------
  -- Edit ticker info
  -----------------------------------------------
  ChangeTickerInfo modal -> case modal of
    StartAdding mOldInfo -> 
      [ Model $ model & #tickerRegistryModel % #editingTicker .~ True -- Show widget.
                      & #tickerRegistryModel % #newTickerInfo .~ fromMaybe def mOldInfo
      ]
    CancelAdding -> 
      [ Model $ model & #tickerRegistryModel % #editingTicker .~ False
                      & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (TickerRegistryEvent . ChangeTickerInfo . AddResult) $ do
          let tickerInfo = tickerRegistryModel ^. #newTickerInfo

          -- Validate the new ticker info.
          verifiedTickerInfo <- fromRightOrAppError $ 
            processNewTickerInfo tickerInfo reverseTickerMap

          -- Add the new ticker to the database.
          insertTickerInfo databaseFile verifiedTickerInfo >>= fromRightOrAppError

          return verifiedTickerInfo
      ]
    AddResult TickerInfo{..} ->
      [ Model $
          model & #tickerRegistryModel % #editingTicker .~ False
                & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
                & #tickerMap %~ Map.insert ticker (policyId,assetName,decimals)
                & #reverseTickerMap %~ Map.insert (policyId,assetName) (ticker,decimals)
      ]

  -----------------------------------------------
  -- Delete the ticker info
  -----------------------------------------------
  DeleteTickerInfo modal -> case modal of
    GetDeleteConfirmation mTarget -> 
      [ Model $ model & #tickerRegistryModel % #deletingTicker .~ True -- Show widget.
                      & #tickerRegistryModel % #newTickerInfo .~ fromMaybe def mTarget
      ]
    CancelDeletion -> 
      [ Model $ model & #tickerRegistryModel % #deletingTicker .~ False -- Close widget.
                      & #tickerRegistryModel % #newTickerInfo .~ def
      ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ TickerRegistryEvent $ DeleteTickerInfo PostDeletionAction) $ do
          let NewTickerInfo{ticker} = tickerRegistryModel ^. #newTickerInfo

          -- Delete the entry.
          deleteTickerInfo databaseFile (Ticker ticker) >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the entry id for the contact to delete.
      let NewTickerInfo{ticker} = tickerRegistryModel ^. #newTickerInfo
          (policy,name,_) = fromMaybe ("","",0) $ Map.lookup (Ticker ticker) tickerMap
      in  [ Model $ 
              model & #tickerRegistryModel % #deletingTicker .~ False
                    & #tickerRegistryModel % #newTickerInfo .~ def
                    & #tickerMap %~ Map.delete (Ticker ticker)
                    & #reverseTickerMap %~ Map.delete (policy,name)
          ]
