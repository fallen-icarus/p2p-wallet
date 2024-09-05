{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel
  ( TxBuilderEvent(..)
  , TxType(..)
  , TxBuilderModel(..)
  , isEmptyBuilder
  , convertToTxBody
  , canBeBuilt
  , hasInputs

  , module P2PWallet.Data.AppModel.TxBuilderModel.ChangeOutput
  , module P2PWallet.Data.AppModel.TxBuilderModel.CollateralInput
  , module P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel
  , module P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel
  , module P2PWallet.Data.AppModel.TxBuilderModel.TestMint
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserCertificate
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserInput
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserOutput
  , module P2PWallet.Data.AppModel.TxBuilderModel.UserWithdrawal
  ) where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.ChangeOutput
import P2PWallet.Data.AppModel.TxBuilderModel.CollateralInput
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel
import P2PWallet.Data.AppModel.TxBuilderModel.TestMint
import P2PWallet.Data.AppModel.TxBuilderModel.UserCertificate
import P2PWallet.Data.AppModel.TxBuilderModel.UserInput
import P2PWallet.Data.AppModel.TxBuilderModel.UserOutput
import P2PWallet.Data.AppModel.TxBuilderModel.UserWithdrawal
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-------------------------------------------------
-- Tx Builder UI Events
-------------------------------------------------
-- | All events unique to the transaction builder. Some events can happen directly from other
-- scenes. Those actions are not here; they are part of the events for those scenes.
data TxBuilderEvent
  -- | Reset all fields in the transaction builder.
  = ResetBuilder
  -- | Open the add popup widget
  | ShowTxAddPopup
  -- | Open the change popup widget
  | ShowTxChangePopup
  -- | Open the collateral popup widget
  | ShowTxCollateralPopup
  -- | Remove the selected user input from the tx builder.
  | RemoveSelectedUserInput Int
  -- | Remove the selected user output from the tx builder.
  | RemoveSelectedUserOutput Int
  -- | Remove the selected user certificate from the tx builder.
  | RemoveSelectedUserCertificate Int
  -- | Remove the selected user withdrawal from the tx builder.
  | RemoveSelectedUserWithdrawal Int
  -- | Remove the test token mint.
  | RemoveTestMint
  -- | Change the desired number of user outputs with these conditions. The first int is the index
  -- into the outputs list and the second is the new count.
  | ChangeUserOutputCount Int Int
  -- | Edit selected user output.
  | EditSelectedUserOutput (AddEvent (Int,UserOutput) (Int,UserOutput))
  -- | Add the new external user output.
  | AddNewExternalUserOutput (AddEvent NewUserOutput UserOutput)
  -- | Add the new change output.
  | AddNewChangeOutput (AddEvent NewChangeOutput ChangeOutput)
  -- | Add the new test mint.
  | AddNewTestMint (AddEvent NewTestMint TestMint)
  -- | Convert the user's input to hexidecimal.
  | ConvertExampleTestMintNameToHexidecimal
  -- | Builder events for swaps.
  | SwapBuilderEvent SwapBuilderEvent
  -- | Builder events for loans.
  | LoanBuilderEvent LoanBuilderEvent
  -- | Build the transaction while also estimating the execution budgets and tx fee. Return the
  -- updated TxBuilderModel.
  | BuildTx (ProcessEvent () TxBuilderModel)
  -- | Witness a transaction that was built using the TxBuilder. Return the absolute filepaths for
  -- the witness files.
  | WitnessTx (ProcessEvent () [KeyWitnessFile])
  -- | Assemble the witness files to produced the signed tx file.
  | AssembleWitnesses (ProcessEvent () SignedTxFile)
  -- | Export the transaction body and any witnesses. It will be exported to the user's home
  -- directory.
  | ExportTxBody (ProcessEvent () FilePath)
  -- | Import an externally signed transaction to submit it through the wallet.
  | ImportSignedTxFile (AddEvent FilePath FilePath)
  -- | Get the export destination for the transaction files.
  | GetTxFileExportDirectory (AddEvent FilePath FilePath)
  deriving (Show,Eq)

-------------------------------------------------
-- Tx Type
-------------------------------------------------
-- | Whether the transaction involves only linked wallets or watched wallets, or if it is a hybrid
-- transaction. What can happen to the built tx.body file depends on the transaction type.
data TxType
  -- | All required keys are known hardware wallet keys.
  = PairedTx
  -- | All required keys are unknown keys.
  | WatchedTx
  -- | Some required keys are known while others are unknown.
  | HybridTx
  deriving (Show,Eq)

instance Semigroup TxType where
  PairedTx <> WatchedTx = HybridTx
  WatchedTx <> PairedTx = HybridTx
  HybridTx <> _ = HybridTx
  _ <> HybridTx = HybridTx
  PairedTx <> PairedTx = PairedTx
  WatchedTx <> WatchedTx = WatchedTx

-------------------------------------------------
-- Builder State
-------------------------------------------------
data TxBuilderModel = TxBuilderModel
  -- | A list of normal UTxOs being spent. These are non-defi inputs. They are indexed to
  -- make editing/deleting easier.
  { userInputs :: [(Int,UserInput)]
  -- | A list of normal UTxO outputs. These are non-defi outputs. They are indexed to
  -- make editing/deleting easier.
  , userOutputs :: [(Int,UserOutput)]
  -- | The target user output being edited.
  , targetUserOutput :: Maybe (Int,NewUserOutput)
  -- | The new external user output information.
  , newExternalUserOutput :: NewUserOutput
  -- | Whether the add external output widget should be open.
  , addingExternalUserOutput :: Bool
  -- | The change output.
  , changeOutput :: Maybe ChangeOutput
  -- | The new change output information.
  , newChangeOutput :: NewChangeOutput
  -- | Whether the add change output widget should be open.
  , addingChangeOutput :: Bool
  -- | A list of certificate actions for the user's stake address. They are indexed to
  -- make deleting easier.
  , userCertificates :: [(Int,UserCertificate)]
  -- | A list of withdrawal actions for the user's stake address. They are indexed to
  -- make editing/deleting easier.
  , userWithdrawals :: [(Int,UserWithdrawal)]
  -- | The test tokens to mint.
  , testMint :: Maybe TestMint
  -- | The new test mint information.
  , newTestMint :: NewTestMint
  -- | Whether the add test mint widget should be open.
  , addingTestMint :: Bool
  -- | The swap builder sub model for swap related builder information.
  , swapBuilderModel :: SwapBuilderModel
  -- | The loans builder sub model for loans related builder information.
  , loanBuilderModel :: LoanBuilderModel
  -- | The transaction fee for the built transaction.
  , fee :: Lovelace
  -- | A list of required witnesses. This is used to determine whether the transaction can be signed
  -- and submitted, or exported. This is set internally by the app.
  , keyWitnesses :: [KeyWitness]
  -- | Whether all witnesses are known hardware wallet keys. If they are, the witness files can be
  -- directly assembled and the final transaction submitted. If not, then the transaction file and
  -- known witnesses must be exported for assembling externally.
  , txType :: TxType
  -- | The key witness files for this transaction.
  , keyWitnessFiles :: [KeyWitnessFile]
  -- | Whether the model is the correct mirror for the tx.body file located in the tmp directory.
  -- This is helpful for knowing whether it is okay to sign, or export.
  , isBuilt :: Bool
  -- | Whether the transaction's assets are balanced.
  , isBalanced :: Bool
  -- | Whether the import signed tx widget should be open.
  , importing :: Bool
  -- | Whether the export directory widget should be open.
  , exporting :: Bool
  -- | User supplied filepath.
  , targetPath :: Text
  -- | Whether the transaction requires collateral.
  , requiresCollateral :: Bool
  -- | The chosen collateral input.
  , collateralInput :: Maybe CollateralInput
  -- | Whether to show the add popup widget.
  , showAddPopup :: Bool
  -- | Whether to show the change popup widget.
  , showChangePopup :: Bool
  -- | Whether to show the collateral popup widget.
  , showCollateralPopup :: Bool
  -- | The current network parameters and the current collateralPercentage.
  , parameters :: Maybe (ByteString, Decimal)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TxBuilderModel

instance Default TxBuilderModel where
  def = TxBuilderModel
    { userInputs = []
    , userOutputs = []
    , targetUserOutput = Nothing
    , newExternalUserOutput = def
    , addingExternalUserOutput = False
    , changeOutput = Nothing
    , newChangeOutput = def
    , addingChangeOutput = False
    , userCertificates = []
    , userWithdrawals = []
    , testMint = Nothing
    , newTestMint = def
    , addingTestMint = False
    , swapBuilderModel = def
    , loanBuilderModel = def
    , fee = 0
    , keyWitnesses = []
    , txType = PairedTx -- This is set so the default GUI button is to submit.
    , keyWitnessFiles = []
    , isBuilt = False
    , isBalanced = False
    , importing = False
    , exporting = False
    , targetPath = ""
    , requiresCollateral = False
    , collateralInput = Nothing
    , showAddPopup = False
    , showChangePopup = False
    , showCollateralPopup = False
    , parameters = Nothing
    }

-- | Check whether the builder has anything yet. Not all fields correspond to the actual
-- transaction (some are for the GUI) so this function only checks the required fields.
isEmptyBuilder :: TxBuilderModel -> Bool
isEmptyBuilder TxBuilderModel{..} = and
  [ null userInputs
  , null userOutputs
  , null userCertificates
  , null userWithdrawals
  , isNothing targetUserOutput
  , isNothing changeOutput
  , isNothing testMint
  , isNothing collateralInput
  , isEmptySwapBuilderModel swapBuilderModel
  , isEmptyLoanBuilderModel loanBuilderModel
  ]

-- | Convert the `TxBuilderModel` to a `TxBody`.
convertToTxBody :: TxBuilderModel -> TxBody
convertToTxBody TxBuilderModel{..} = 
  -- A strict fold that merges all pieces. The `Semigroup` instance can be found in 
  -- `P2PWallet.Data.Core.TxBody`.
  foldMap' id 
    -- Add the normal user inputs to the list of inputs.
    [ foldMap' (addToTxBody mempty . snd) userInputs
    -- Add the normal user outputs to the list of outputs.
    , foldMap' (addToTxBody mempty . snd) userOutputs
    -- Add the change output to the list of outputs.
    , maybe mempty (addToTxBody mempty) changeOutput
    -- Add the certificates to the list of certificates.
    , foldMap' (addToTxBody mempty . snd) userCertificates
    -- Add the withdrawals to the list of withdrawals.
    , foldMap' (addToTxBody mempty . snd) userWithdrawals
    -- Add the testMint to the list of mints.
    , maybe mempty (addToTxBody mempty) testMint
    -- Add any swap actions.
    , addToTxBody mempty swapBuilderModel
    -- Add any loan actions.
    , addToTxBody mempty loanBuilderModel
    -- Add the collateral and the fee to the transaction. This is done together because
    -- the amount of collateral required depends on the fee.
    , let txWithFee = mempty & #fee .~ fee in
      maybe txWithFee (addToTxBody txWithFee) collateralInput
    ]

-- | Since `cardano-cli transaction build-raw` can throw confusing error messages if certain pieces
-- are missing from the transaction, this check validates those cases will not occur. 
canBeBuilt :: TxBuilderModel -> Either Text ()
canBeBuilt tx@TxBuilderModel{..}
  | not $ hasInputs tx = Left "No inputs specified."
  -- A change address must be specified.
  | change ^. #paymentAddress == "" = Left "Change address missing."
  -- The value of ADA must be balanced.
  | change ^. #lovelace < 0 = Left "Ada is not balanced."
  -- The value of the native assets must be balanced.
  | nativeAssetsNotBalanced = Left "Native assets are not balanced."
  -- If it requires collateral, then a collateral input must be set.
  | requiresCollateral && isNothing collateralInput = Left "Transaction requires collateral."
  | otherwise = return ()
  where
    change :: ChangeOutput
    change = fromMaybe def changeOutput

    nativeAssetsNotBalanced :: Bool
    nativeAssetsNotBalanced = any ((<0) . view #quantity) $ change ^. #nativeAssets

-- | This is used by the status bar for the tx builder.
hasInputs :: TxBuilderModel -> Bool
hasInputs TxBuilderModel{..} = or
  [ userInputs /= []
  , swapBuilderModel ^. #swapCloses /= []
  , swapBuilderModel ^. #swapUpdates /= []
  , swapBuilderModel ^. #swapExecutions /= []
  , loanBuilderModel ^. #askCloses /= []
  , loanBuilderModel ^. #askUpdates /= []
  , loanBuilderModel ^. #offerCloses /= []
  , loanBuilderModel ^. #offerUpdates /= []
  , loanBuilderModel ^. #offerAcceptances /= []
  , loanBuilderModel ^. #loanPayments /= []
  , loanBuilderModel ^. #interestApplications /= []
  , loanBuilderModel ^. #expiredClaims /= []
  ]
