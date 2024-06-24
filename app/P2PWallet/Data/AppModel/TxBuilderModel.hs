{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel
  ( TxBuilderScene(..)
  , TxBuilderEvent(..)
  , TxBuilderModel(..)
  , isEmptyBuilder
  , convertToTxBody

  , module P2PWallet.Data.AppModel.TxBuilderModel.ChangeOutput
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserInput
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserOutput
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.ChangeOutput
import P2PWallet.Data.AppModel.TxBuilderModel.UserInput
import P2PWallet.Data.AppModel.TxBuilderModel.UserOutput
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
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
  -- | Build the transaction while also estimating the execution budgets and tx fee. Return the
  -- updated TxBuilderModel.
  | BuildTx (ProcessEvent TxBuilderModel)
  -- | Witness a transaction that was built using the TxBuilder. Return the absolute filepaths for
  -- the witness files.
  | WitnessTx (ProcessEvent [WitnessFile])
  -- | Assemble the witness files to produced the signed tx file.
  | AssembleWitnesses (ProcessEvent SignedTxFile)
  -- | Export the transaction body and any witnesses. It will be exported to the user's home
  -- directory.
  | ExportTxBody (ProcessEvent FilePath)
  -- | Import an externally signed transaction to submit it through the wallet.
  | ImportSignedTxFile (AddEvent FilePath FilePath)
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
  -- | A list of required witnesses. This is used to determine whether the transaction can be signed
  -- and submitted, or exported. This is set internally by the app.
  , witnesses :: [Witness]
  -- | Whether all witnesses are known hardware wallet keys. If they are, the witness files can be
  -- directly assembled and the final transaction submitted. If not, then the transaction file and
  -- known witnesses must be exported for assembling externally.
  , allWitnessesKnown :: Bool
  -- | The witness files for this transaction.
  , witnessFiles :: [WitnessFile]
  -- | Whether the model is the correct mirror for the tx.body file located in the tmp directory.
  -- This is helpful for knowing whether it is okay to sign, or export.
  , isBuilt :: Bool
  -- | Whether the transaction's assets are balanced.
  , isBalanced :: Bool
  -- | Whether the import signed tx widget should be open.
  , importing :: Bool
  -- | The file path to the imported tx.signed.
  , importedSignedTxFile :: Text
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
    , witnesses = []
    , allWitnessesKnown = True -- This is set to true so the default GUI button is to submit.
    , witnessFiles = []
    , isBuilt = False
    , isBalanced = False
    , importing = False
    , importedSignedTxFile = ""
    , requiresCollateral = False
    , collateralInput = Nothing
    , showAddPopup = False
    , showChangePopup = False
    }

-- | Check whether the builder has anything yet. Not all fields correspond to the actual
-- transaction so this function only checks the related fields.
isEmptyBuilder :: TxBuilderModel -> Bool
isEmptyBuilder TxBuilderModel{..} = and
  [ null userInputs
  , null userOutputs
  , isNothing targetUserOutput
  , isNothing changeOutput
  ]

-- | Convert the `TxBuilderModel` to a `TxBody`.
convertToTxBody :: TxBuilderModel -> TxBody
convertToTxBody TxBuilderModel{..} = foldMap' id -- a strict fold that merges all pieces
  -- Add the normal user inputs to the list of inputs.
  [ foldMap' (addToTxBody mempty . snd) userInputs
  -- Add the normal user outputs to the list of outputs.
  , foldMap' (addToTxBody mempty . snd) userOutputs
  -- Add the change output to the list of outputs.
  , maybe mempty (addToTxBody mempty) changeOutput
  -- Overide the fee with the fee in the builder model.
  , mempty & #fee .~ fee
  -- Get the spending witnesses. Duplicates will be removed with the final `foldMap' id`.
  , mempty & #witnesses .~ mapMaybe (requiredWitness . snd) userInputs
  ]
