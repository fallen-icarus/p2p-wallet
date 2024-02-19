{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.App.TxBuilder where

import Prettyprinter

import P2PWallet.Prelude
import P2PWallet.Data.Core
import P2PWallet.Data.Plutus
import P2PWallet.Data.Wallets

-------------------------------------------------
-- Inputs
-------------------------------------------------
-- | Information for a particular input.
data VerifiedInput = VerifiedInput
  { _utxoRef :: TxOutRef
  -- | The bech32 address for this input. This is used to get any required key witnesses.
  , _paymentAddress :: PaymentAddress
  -- | The path to the required hw key for witnessing.
  , _paymentKeyPath :: Maybe DerivationPath 
  , _lovelaces :: Lovelace
  , _nativeAssets :: [NativeAsset]
  -- | Whether the widget expands the info for this input.
  , _expanded :: Bool 
  } deriving (Show,Eq)

instance Pretty VerifiedInput where
  pretty VerifiedInput{..} = align $
    vsep [ "Output Reference:" <+> pretty @Text (showTxOutRef _utxoRef)
         , "Value:" <+> pretty @String (printf "%D ADA" $ toADA _lovelaces)
         , if null _nativeAssets 
           then "Native Assets: none"
           else vsep [ "Native Assets:"
                     , indent 4 $ align $ vsep $ map 
                         (\NativeAsset{..} -> 
                            pretty _quantity <+> pretty (_policyId <> "." <> _tokenName))
                         _nativeAssets
                     ]
         ]

-- | Input information that must be supplied by the user.
data UserInput = UserInput
  { _utxoRef :: Text
  } deriving (Show,Eq)

instance Default UserInput where
  def = UserInput
    { _utxoRef = ""
    }

-- | Verify the user supplied fields for an input. Populate the other fields with dummy values.
toVerifiedInput :: UserInput -> Either Text VerifiedInput
toVerifiedInput UserInput{..} = do
  utxoRef <- maybeToRight "UTxO reference is not a valid output reference" $ readTxOutRef _utxoRef
  return $
    VerifiedInput
      { _utxoRef = utxoRef
      , _paymentAddress = ""
      , _paymentKeyPath = Nothing
      , _lovelaces = 0
      , _nativeAssets = []
      , _expanded = False
      }

-- | Convert a verified input back to a user input to allow the user to edit it.
fromVerifiedInput :: VerifiedInput -> UserInput
fromVerifiedInput VerifiedInput{..} =
  UserInput
    { _utxoRef = showTxOutRef _utxoRef
    }

-------------------------------------------------
-- Outputs
-------------------------------------------------
-- | Information for a particular output.
data VerifiedOutput = VerifiedOutput
  -- | This field is not strictly necessary but is useful for quickly converting back to
  -- `UserOutput` for editing.
  { _internalWallet :: Maybe PaymentWallet
  , _paymentAddress :: PaymentAddress
  , _lovelaces :: Lovelace
  , _nativeAssets :: [NativeAsset]
  , _expanded :: Bool -- ^ Whether the widget expands the info for this output.
  } deriving (Show,Eq)

instance Pretty VerifiedOutput where
  pretty VerifiedOutput{..} = align $
    vsep [ if _paymentAddress == "" then "Payment Address: none" else
             "Payment Address:" <+> pretty _paymentAddress
         , "Value:" <+> pretty @String (printf "%D ADA" $ toADA _lovelaces)
         , if null _nativeAssets 
           then "Native Assets: none"
           else vsep [ "Native Assets:"
                     , indent 4 $ align $ vsep $ map 
                         (\NativeAsset{..} -> 
                            pretty _quantity <+> pretty (_policyId <> "." <> _tokenName))
                         _nativeAssets
                     ]
         ]

data UserOutput = UserOutput
  -- | `Just wallet` if the output is to a tracked payment wallet and `Nothing` if the
  -- output is to an external address. This is useful for allowing the user to pick from a
  -- dropdown menu of tracked wallets.
  { _internalWallet :: Maybe PaymentWallet
  -- | The bech32 payment address. If the `_internalWallet` is `Just`, then this field can be
  -- the empty string.
  , _paymentAddress :: Text
  , _lovelaces :: Integer
  -- | The native assets of the form "# policy_id.token_name". The native assets must be separated
  -- by newlines.
  , _nativeAssets :: Text
  } deriving (Show,Eq)

instance Default UserOutput where
  def = UserOutput
    { _internalWallet = Nothing
    , _paymentAddress = ""
    , _lovelaces = 0
    , _nativeAssets = ""
    } 

-- | Verify the user supplied fields for an output. Populate the other fields with dummy values.
toVerifiedOutput :: Network -> UserOutput -> Either Text VerifiedOutput
toVerifiedOutput network UserOutput{..} = do
    -- If the user selected an internal wallet, the address field will be blank. This scenario
    -- should not throw an error.
    addr <- case _internalWallet of
      Nothing -> readPaymentAddress network _paymentAddress
      Just PaymentWallet{_paymentAddress = payAddr} -> return payAddr

    -- Check that the ada balance is positive.
    when (_lovelaces < 0) $ Left "Outputs must have positive values of ADA."

    -- Check that the assets are valid. Returns the first error, if any.
    assets <- sequence $ map parseNativeAsset $ lines _nativeAssets

    return $
      VerifiedOutput
        { _internalWallet = _internalWallet
        , _paymentAddress = addr
        , _lovelaces = Lovelace _lovelaces
        , _nativeAssets = assets
        , _expanded = False
        }
  where
    -- Native assets are supposed to be of the form '# policy_id.asset_name' and separated
    -- by newlines. All quantities must be greater than or equal to 0.
    parseNativeAsset :: Text -> Either Text NativeAsset
    parseNativeAsset assetLine = do
      asset@NativeAsset{_quantity} <- flip maybeToRight (readNativeAsset assetLine) $
        unlines
          [ "Invalid native asset entry. Must be of the form '# policy_id.asset_name'."
          , "Native assets must be separated by newlines."
          , ""
          , "Could not parse: '" <> assetLine <> "'"
          ]
      if _quantity >= 0 then Right asset else 
        Left $
          unlines 
            [ "Native asset quantities must be greater than or equal to 0."
            , ""
            , "Invalid quantity: '" <> assetLine <> "'"
            ]

-- | Convert a verified output back to a user output to allow the user to edit it.
fromVerifiedOutput :: VerifiedOutput -> UserOutput
fromVerifiedOutput VerifiedOutput{..} =
  UserOutput
    { _internalWallet = _internalWallet
    , _paymentAddress = unPaymentAddress _paymentAddress
    , _lovelaces = unLovelace _lovelaces
    , _nativeAssets = 
        unlines $ flip map _nativeAssets $ \NativeAsset{..} -> 
          show _quantity <> " " <> _policyId <> "." <> _tokenName
    }

-------------------------------------------------
-- Change Output
-------------------------------------------------
-- | Information for the change output. The fee will be deducted from this output.
data VerifiedChangeOutput = VerifiedChangeOutput
  -- | This field is not strictly necessary but is useful for quickly converting back to
  -- `UserChangeOutput` for editing.
  { _internalWallet :: Maybe PaymentWallet
  , _paymentAddress :: PaymentAddress
  , _lovelaces :: Lovelace
  , _nativeAssets :: [NativeAsset]
  , _expanded :: Bool -- ^ Whether the widget expands the info for this output.
  } deriving (Show,Eq)

instance Default VerifiedChangeOutput where
  def = VerifiedChangeOutput
    { _internalWallet = Nothing
    , _paymentAddress = ""
    , _lovelaces = 0
    , _nativeAssets = []
    , _expanded = True
    } 

instance Pretty VerifiedChangeOutput where
  pretty VerifiedChangeOutput{..} = align $
    vsep [ if _paymentAddress == "" then "Payment Address: none" else
             "Payment Address:" <+> pretty _paymentAddress
         , "Value:" <+> pretty @String (printf "%D ADA" $ toADA _lovelaces)
         , if null _nativeAssets 
           then "Native Assets: none"
           else vsep [ "Native Assets:"
                     , indent 4 $ align $ vsep $ map 
                         (\NativeAsset{..} -> 
                            pretty _quantity <+> pretty (_policyId <> "." <> _tokenName))
                         _nativeAssets
                     ]
         ]

data UserChangeOutput = UserChangeOutput
  -- | `Just wallet` if the output is to a tracked payment wallet and `Nothing` if the
  -- output is to an external address. This is useful for allowing the user to pick from a
  -- dropdown menu of tracked wallets.
  { _internalWallet :: Maybe PaymentWallet
  -- | The bech32 payment address. If the `_internalWallet` is `Just`, then this field can be
  -- the empty string.
  , _paymentAddress :: Text
  } deriving (Show,Eq)

instance Default UserChangeOutput where
  def = UserChangeOutput
    { _internalWallet = Nothing
    , _paymentAddress = ""
    } 

-- | Verify the user supplied fields for a change output. Populate the other fields with dummy 
-- values.
toVerifiedChangeOutput :: Network -> UserChangeOutput -> Either Text VerifiedChangeOutput
toVerifiedChangeOutput network UserChangeOutput{..} = do
    -- If the user selected an internal wallet, the address field will be blank. This scenario
    -- should not throw an error.
    addr <- case _internalWallet of
      Nothing -> readPaymentAddress network _paymentAddress
      Just PaymentWallet{_paymentAddress = payAddr} -> return payAddr

    return $
      VerifiedChangeOutput
        { _internalWallet = _internalWallet
        , _paymentAddress = addr
        , _lovelaces = 0
        , _nativeAssets = []
        , _expanded = True
        }

-- | Convert a verified change output back to a user change output to allow the user 
-- to edit it.
fromVerifiedChangeOutput :: VerifiedChangeOutput -> UserChangeOutput
fromVerifiedChangeOutput VerifiedChangeOutput{..} =
  UserChangeOutput
    { _internalWallet = _internalWallet
    , _paymentAddress = unPaymentAddress _paymentAddress
    }

-------------------------------------------------
-- Certificates
-------------------------------------------------
-- | Types of certificate actions. When registering and delegating in the same transaction,
-- the registration certificate MUST be first.
data CertificateAction
  -- | Register a staking credential and pay the 2 ADA deposit.
  = Registration
  -- | Deregister a staking credential and recover the 2 ADA deposit.
  | Deregistration
  -- | Delegate the staking credential.
  | Delegation PoolID
  deriving (Show,Eq,Ord)

instance Pretty CertificateAction where
  pretty Registration = "Registration"
  pretty Deregistration = "Deregistration"
  pretty (Delegation (PoolID poolId)) = "Delegation to " <> pretty poolId

-- | The type representing user supplied information for a certificate.
data UserCertificate = UserCertificate
  -- | `Just wallet` if the output is to a tracked stake wallet and `Nothing` if the
  -- output is to an external address. This is useful for allowing the user to pick from a
  -- dropdown menu of tracked wallets.
  { _internalWallet :: Maybe StakeWallet
  -- | The bech32 stake address. If the `_internalWallet` is `Just`, then this field can be
  -- the empty string.
  , _stakeAddress :: Text
  -- | What kind of certificate this is.
  , _certificateAction :: CertificateAction
  } deriving (Show,Eq)

instance Default UserCertificate where
  def = UserCertificate
    { _internalWallet = Nothing
    , _stakeAddress = ""
    , _certificateAction = Registration
    } 

-- | The type representing verified information for a certificate.
data VerifiedCertificate = VerifiedCertificate
  -- | This field is not strictly necessary but is useful for quickly converting back to
  -- `UserCertificate` for editing.
  { _internalWallet :: Maybe StakeWallet
  -- | The bech32 stake address. 
  , _stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , _stakeKeyPath :: Maybe DerivationPath
  -- | What kind of certificate this is.
  , _certificateAction :: CertificateAction
  -- | Whether the widget expands the info for this certificate.
  , _expanded :: Bool
  } deriving (Show,Eq)

instance Default VerifiedCertificate where
  def = VerifiedCertificate
    { _internalWallet = Nothing
    , _stakeAddress = ""
    , _stakeKeyPath = Nothing
    , _certificateAction = Registration
    , _expanded = False
    } 

instance Pretty VerifiedCertificate where
  pretty VerifiedCertificate{..} = align $
    vsep [ "Stake Address:" <+> pretty _stakeAddress
         , "Purpose:" <+> pretty _certificateAction
         ]

-- | Verify the user supplied fields for a certificate. Populate the other fields with dummy values.
toVerifiedCertificate :: Network -> UserCertificate -> Either Text VerifiedCertificate
toVerifiedCertificate network UserCertificate{..} = do
    -- If the user selected an internal wallet, the address field will be blank. This scenario
    -- should not throw an error.
    (addr,keyPath) <- case _internalWallet of
      Nothing -> (,Nothing) <$> readStakeAddress network _stakeAddress
      Just StakeWallet{_stakeAddress = stakeAddr,_stakeKeyPath = sKeyPath} -> 
        return (stakeAddr,sKeyPath)

    return $
      VerifiedCertificate
        { _internalWallet = _internalWallet
        , _stakeAddress = addr
        , _stakeKeyPath = keyPath
        , _certificateAction = _certificateAction
        , _expanded = False
        }

-- | Convert a verified certificate back to a user certificate to allow the user 
-- to edit it.
fromVerifiedCertificate :: VerifiedCertificate -> UserCertificate
fromVerifiedCertificate VerifiedCertificate{..} =
  UserCertificate
    { _internalWallet = _internalWallet
    , _stakeAddress = unStakeAddress _stakeAddress
    , _certificateAction = _certificateAction
    }

-------------------------------------------------
-- Withdrawals
-------------------------------------------------
-- | The type representing user supplied information for a withdrawal.
data UserWithdrawal = UserWithdrawal
  -- | `Just wallet` if the output is to a tracked stake wallet and `Nothing` if the
  -- output is to an external address. This is useful for allowing the user to pick from a
  -- dropdown menu of tracked wallets.
  { _internalWallet :: Maybe StakeWallet
  -- | The bech32 stake address. If the `_internalWallet` is `Just`, then this field can be
  -- the empty string.
  , _stakeAddress :: Text
  -- | The amount of lovelace withdrawn.
  , _lovelaces :: Integer
  } deriving (Show,Eq)

instance Default UserWithdrawal where
  def = UserWithdrawal
    { _internalWallet = Nothing
    , _stakeAddress = ""
    , _lovelaces = 0
    } 

-- | Information for a particular withdrawal.
data VerifiedWithdrawal = VerifiedWithdrawal
  -- | This field is not strictly necessary but is useful for quickly converting back to
  -- `UserWithdrawal` for editing.
  { _internalWallet :: Maybe StakeWallet
  -- | The bech32 stake address. 
  , _stakeAddress :: StakeAddress
  -- | The path to the required hw key for witnessing.
  , _stakeKeyPath :: Maybe DerivationPath
  -- | The amount withdrawn.
  , _lovelaces :: Lovelace
  -- | Whether the widget expands the info for this withdrawal.
  , _expanded :: Bool
  } deriving (Show,Eq)

instance Default VerifiedWithdrawal where
  def = VerifiedWithdrawal
    { _internalWallet = Nothing
    , _stakeAddress = ""
    , _stakeKeyPath = Nothing
    , _lovelaces = 0
    , _expanded = False
    }

instance Pretty VerifiedWithdrawal where
  pretty VerifiedWithdrawal{..} = align $
    vsep [ "Stake Address:" <+> pretty _stakeAddress
         , "Value:" <+> pretty @String (printf "%D ADA" $ toADA _lovelaces)
         ]

-- | Verify the user supplied fields for a withdrawal. Populate the other fields with dummy values.
toVerifiedWithdrawal :: Network -> UserWithdrawal -> Either Text VerifiedWithdrawal
toVerifiedWithdrawal network UserWithdrawal{..} = do
    -- If the user selected an internal wallet, the address field will be blank. This scenario
    -- should not throw an error.
    (addr,keyPath) <- case _internalWallet of
      Nothing -> (,Nothing) <$> readStakeAddress network _stakeAddress
      Just StakeWallet{_stakeAddress = stakeAddr,_stakeKeyPath = sKeyPath} -> 
        return (stakeAddr,sKeyPath)

    when (_lovelaces < 0) $ Left "Withdrawal values must be >= 0."

    return $
      VerifiedWithdrawal
        { _internalWallet = _internalWallet
        , _stakeAddress = addr
        , _stakeKeyPath = keyPath
        , _lovelaces = Lovelace _lovelaces
        , _expanded = False
        }

-- | Convert a verified withdrawal back to a user withdrawal to allow the user 
-- to edit it.
fromVerifiedWithdrawal :: VerifiedWithdrawal -> UserWithdrawal
fromVerifiedWithdrawal VerifiedWithdrawal{..} =
  UserWithdrawal
    { _internalWallet = _internalWallet
    , _stakeAddress = toText _stakeAddress
    , _lovelaces = unLovelace _lovelaces
    }

-------------------------------------------------
-- Tx Builder subscenes
-------------------------------------------------
-- | The subscenes for the TxBuilder.
data BuilderScene
  -- | Show the current status of the transaction.
  = BuilderSummary
  -- | Open the widget for adding/editing outputs.
  | BuilderAddNewOutput
  -- | Open the widget for adding/editing inputs.
  | BuilderAddNewInput
  -- | Open the widget for adding/editing the change output.
  | BuilderAddChangeOutput
  -- | Open the widget for adding/editing certificates.
  | BuilderAddNewCertificate
  -- | Open the widget for getting the export destination for exporting the
  -- tx.body file.
  | BuilderGetExportDestination
  deriving (Show,Eq)

-------------------------------------------------
-- Tx Builder UI Events
-------------------------------------------------
-- | All events unique to the transaction builder. Signing and submitting
-- transactions can also be done for externally built transactions which
-- is why they are not included here.
data TxBuilderEvent
  -- | Change the subscene to the specified scene.
  = ChangeBuilderScene BuilderScene 
  -- | Validate and insert the new output.
  | InsertNewOutput
  -- | Validate and insert the new input.
  | InsertNewInput
  -- | Validate and insert the new change output.
  | InsertNewChangeOutput
  -- | Validate and insert the new certificate.
  | InsertNewCertificate
  -- | Validate and insert the new withdrawal.
  | InsertNewWithdrawal
  -- | Delete the input with the specified index.
  | DeleteInput Int
  -- | Delete the output with the specified index.
  | DeleteOutput Int
  -- | Delete the certificate with the specified index.
  | DeleteCertificate Int
  -- | Delete the withdrawal with the specified index.
  | DeleteWithdrawal Int
  -- | Edit the change output.
  | EditChangeOutput
  -- | Edit the output with the specified index.
  | EditOutput Int
  -- | Edit the input with the specified index.
  | EditInput Int
  -- | Edit the certificate with the specified index.
  | EditCertificate Int
  -- | Edit the withdrawal with the specified index.
  | EditWithdrawal Int
  -- | Build the transaction while also estimating the execution budgets and tx fee.
  | BuildTx
  -- | The updated transaction model with the new transaction fee.
  | BuildResult TxBuilderModel
  -- | Reset all fields in the transaction builder.
  | ResetBuilder
  -- | Save the tx.body file to an external location.
  | ExportTxBody
  -- | Close the export widget if successfull or else just throw the error message.
  | ExportTxBodyResult Text
  deriving (Show,Eq)

-------------------------------------------------
-- TxBuilder model
-------------------------------------------------
data TxBuilderModel = TxBuilderModel
  -- | The current subscene.
  { _scene :: BuilderScene
  -- | The new output. If the index is 0, then this is a new output, otherwise it is editing
  -- the `VerifiedOutput` with the corresponding index.
  , _newOutput :: (Int,UserOutput)
  -- | The list of `VerifiedOutput`s.
  , _outputs :: [(Int,VerifiedOutput)] 
  -- | The new input. If the index is 0, then this is a new input, otherwise it is editing
  -- the `VerifiedInput` with the corresponding index.
  , _newInput :: (Int,UserInput)
  -- | The list of `VerifiedInput`s. These will always be sorted lexicographically. The indexes
  -- will be updated after sorting each time an input is added/removed.
  , _inputs :: [(Int,VerifiedInput)]
  -- | The new change output.
  , _newChangeOutput :: UserChangeOutput
  -- | The verified change output.
  , _changeOutput :: VerifiedChangeOutput
  -- | The new certificate. If the index is 0, then this is a new certificate, otherwise it is 
  -- editing the `VerifiedCertificate` with the corresponding index.
  , _newCertificate :: (Int,UserCertificate)
  -- | The list of `VerifiedCertificate`s. 
  , _certificates :: [(Int,VerifiedCertificate)]
  -- | The new withdrawal. If the index is 0, then this is a new withdrawal, otherwise it is 
  -- editing the `VerifiedWithdrawal` with the corresponding index.
  , _newWithdrawal :: (Int,UserWithdrawal)
  -- | The list of `VerifiedWithdrawal`s. 
  , _withdrawals :: [(Int,VerifiedWithdrawal)]
  -- | The transaction fee. This is automatically calculated and set when building the transaction.
  , _txFee :: Lovelace
  -- | Whether the model is the correct mirror for the tx.body file located in the tmp directory.
  -- This is helpful for knowing whether it is okay to sign and submit, or export.
  , _isBuilt :: Bool
  } deriving (Show,Eq)

instance Default TxBuilderModel where
  def = TxBuilderModel
    { _scene = BuilderSummary
    , _newOutput = (0,def)
    , _newInput = (0,def)
    , _newCertificate = (0,def)
    , _newWithdrawal = (0,def)
    , _outputs = []
    , _inputs = []
    , _certificates = []
    , _withdrawals = []
    , _newChangeOutput = def
    , _changeOutput = def
    , _txFee = 0
    , _isBuilt = False
    }
