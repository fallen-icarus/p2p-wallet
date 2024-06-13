{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.TickerRegistryEvent
  ( 
    handleTickerRegistryEvent
  ) where

import Monomer hiding (decimals)
import Data.Map qualified as Map

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.TickerMap
import P2PWallet.Data.AppModel
import P2PWallet.Plutus
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
          addNewTickerInfo databaseFile verifiedTickerInfo >>= fromRightOrAppError

          return verifiedTickerInfo
      ]
    AddResult TickerInfo{..} ->
      [ Model $
          model & #tickerRegistryModel % #addingTicker .~ False
                & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
                & #tickerMap %~ Map.insert ticker (policyId,assetName,decimals)
                & #reverseTickerMap %~ 
                    Map.insert (policyId <> "." <> assetName) (ticker,decimals)
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
          let tickerInfo@NewTickerInfo{policyId,assetName} = tickerRegistryModel ^. #newTickerInfo
              -- Delete the on-chain entry in the reverse map.
              restOfReverseMap = Map.delete (policyId <> "." <> assetName) reverseTickerMap

          -- Validate the new ticker info.
          verifiedTickerInfo <- fromRightOrAppError $ 
            processNewTickerInfo tickerInfo restOfReverseMap

          -- Add the new ticker to the database.
          addNewTickerInfo databaseFile verifiedTickerInfo >>= fromRightOrAppError

          -- The new reverse map is returned to ensure the old on-chain name is no longer present.
          return (verifiedTickerInfo,restOfReverseMap)
      ]
    AddResult (TickerInfo{..},restOfReverseMap) ->
      [ Model $
          model & #tickerRegistryModel % #editingTicker .~ False
                & #tickerRegistryModel % #newTickerInfo .~ def -- Clear for next time.
                & #tickerMap %~ Map.insert ticker (policyId,assetName,decimals)
                & #reverseTickerMap .~ 
                    Map.insert (policyId <> "." <> assetName) (ticker,decimals) restOfReverseMap
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
          -- Get the entry id for the contact to delete.
          let NewTickerInfo{ticker} = tickerRegistryModel ^. #newTickerInfo

          -- Delete the entry.
          deleteTickerInfo databaseFile ticker >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the entry id for the contact to delete.
      let NewTickerInfo{..} = tickerRegistryModel ^. #newTickerInfo
      in  [ Model $ 
              model & #tickerRegistryModel % #deletingTicker .~ False
                    & #tickerRegistryModel % #newTickerInfo .~ def
                    & #tickerMap %~ Map.delete ticker
                    & #reverseTickerMap %~ Map.delete (policyId <> "." <> assetName)
          ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
processNewTickerInfo :: NewTickerInfo -> ReverseTickerMap -> Either Text TickerInfo
processNewTickerInfo NewTickerInfo{..} reverseTickerMap = do
  -- Check the policy id is valid.
  void $ maybeToRight "Not a valid policy id" $ readHex policyId

  -- Check the asset name is valid.
  void $ maybeToRight "Not a valid asset name" $ readHex assetName

  -- Check the on-chain name is not already linked to another ticker.
  first (\(tckr,_) -> "This asset is already linked to another ticker: '" <> tckr <> "'") $
    maybeToLeft () $ Map.lookup (policyId <> "." <> assetName) reverseTickerMap

  -- Check the decimal places is valid.
  when (decimals < 0) $ Left "Decimal places must be >= 0"

  return $ TickerInfo
    { ticker = ticker
    , policyId = policyId
    , assetName = assetName
    , decimals = decimals
    }
