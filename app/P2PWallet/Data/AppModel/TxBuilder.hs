{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilder 
  ( TxBuilderScene(..)
  , TxBuilderEvent(..)
  , TxBuilderModel(..)
  , isEmptyBuilder

  , module P2PWallet.Data.AppModel.TxBuilder.ChangeOutput
  , module P2PWallet.Data.AppModel.TxBuilder.UserInput
  , module P2PWallet.Data.AppModel.TxBuilder.UserOutput
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilder.ChangeOutput
import P2PWallet.Data.AppModel.TxBuilder.UserInput
import P2PWallet.Data.AppModel.TxBuilder.UserOutput
import P2PWallet.Data.Core.Asset
import P2PWallet.Prelude

-------------------------------------------------
-- TxBuilder Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Tx Builder page.
data TxBuilderScene
  -- | Show the builder in a simple manner. This only shows high-level actions like: creating a
  -- swap and sending 10 ADA to Bob.
  = TxBuilderSimpleView
  -- | Show what the actual transaction looks like. This shows the low-level actions like:
  -- executing minting policy x to mint beacons a, b, and c.
  | TxBuilderAdvancedView 
  deriving (Eq,Show)

-------------------------------------------------
-- Tx Builder UI Events
-------------------------------------------------
-- | All events unique to the transaction builder. Some events can happen directly from other
-- scenes. Those actions are not here; they are part of the events for those scenes.
data TxBuilderEvent
  -- | Change the subscene to the specified scene.
  = ChangeBuilderScene TxBuilderScene 
  -- | Reset all fields in the transaction builder.
  | ResetBuilder
  -- | Open the add popup widget
  | ShowTxAddPopup
  -- | Open the change popup widget
  | ShowTxChangePopup
  -- | Remove the selected user input from the tx builder.
  | RemoveSelectedUserInput Int
  -- | Remove the selected user output from the tx builder.
  | RemoveSelectedUserOutput Int
  -- | Change the desired number of user outputs with these conditions. The first int is the index
  -- into the outputs list and the second is the new count.
  | ChangeUserOutputCount Int Int
  -- | Edit selected user output.
  | EditSelectedUserOutput (AddEvent (Int,UserOutput) (Int,UserOutput))
  -- | Add the new change output.
  | AddNewChangeOutput (AddEvent NewChangeOutput ChangeOutput)
  -- | Build the transaction while also estimating the execution budgets and tx fee.
  | BuildTx
  -- | The updated transaction model with the new transaction fee.
  | BuildResult TxBuilderModel
  -- | Sign a transaction that was built using the TxBuilder, and then immediately submit it.
  | SignAndSubmitTx
  deriving (Show,Eq)

-------------------------------------------------
-- Builder State
-------------------------------------------------
data TxBuilderModel = TxBuilderModel
  { scene :: TxBuilderScene
  -- | A list of normal UTxOs being spent. These are non-defi inputs. They are indexed to
  -- make editing/deleting easier.
  , userInputs :: [(Int,UserInput)]
  -- | A list of normal UTxO outputs. These are non-defi outputs. They are indexed to
  -- make editing/deleting easier.
  , userOutputs :: [(Int,UserOutput)]
  -- | The target user output being edited.
  , targetUserOutput :: Maybe (Int,NewUserOutput)
  -- | The change output.
  , changeOutput :: Maybe ChangeOutput
  -- | The new change output information.
  , newChangeOutput :: NewChangeOutput
  -- | Whether the add change output widget should be open.
  , addingChangeOutput :: Bool
  -- | The transaction fee for the built transaction.
  , fee :: Lovelace
  -- | Whether the model is the correct mirror for the tx.body file located in the tmp directory.
  -- This is helpful for knowing whether it is okay to sign, or export.
  , isBuilt :: Bool
  -- | Whether the transaction's assets are balanced.
  , isBalanced :: Bool
  -- | Whether the transaction requires collateral.
  , requiresCollateral :: Bool
  -- | The chosen collateral input.
  , collateralInput :: Maybe UserInput
  -- | Whether to show the add popup widget.
  , showAddPopup :: Bool
  -- | Whether to show the change popup widget.
  , showChangePopup :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBuilderModel

instance Default TxBuilderModel where
  def = TxBuilderModel
    { scene = TxBuilderSimpleView
    , userInputs = []
    , userOutputs = []
    , targetUserOutput = Nothing
    , changeOutput = Nothing
    , newChangeOutput = def
    , addingChangeOutput = False
    , fee = 0
    , isBuilt = False
    , isBalanced = False
    , requiresCollateral = False
    , collateralInput = Nothing
    , showAddPopup = False
    , showChangePopup = False
    }

-- | Check whether the builder has anything yet. Not all fields correspond to the actual
-- transaction so this function only checks the related fields.
isEmptyBuilder :: TxBuilderModel -> Bool
isEmptyBuilder TxBuilderModel{..} = and
  [ userInputs == []
  , userOutputs == []
  , targetUserOutput == Nothing
  , changeOutput == Nothing
  ]
