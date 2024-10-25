{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.OptionsWallet where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import Database.SQLite.Simple (ToRow,FromRow)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Database
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Options Datum
-------------------------------------------------
-- | The options datum used for the options address UTxO.
data OptionsDatum
  = OptionsProposalDatum Options.ProposalDatum
  | OptionsActiveDatum Options.ActiveDatum
  -- | This is included in case a bug is found in the protocols.
  | OptionsDatumError Value
  deriving (Eq,Show)

makePrisms ''OptionsDatum

instance FromJSON OptionsDatum where
  parseJSON value = pure $ fromMaybe (OptionsDatumError value) $ asum
    [ OptionsProposalDatum <$> parseMaybe (parseJSON @Options.ProposalDatum) value
    , OptionsActiveDatum <$> parseMaybe (parseJSON @Options.ActiveDatum) value
    ]

instance ToJSON OptionsDatum where
  toJSON (OptionsProposalDatum datum) = toJSON datum
  toJSON (OptionsActiveDatum datum) = toJSON datum
  toJSON (OptionsDatumError value) = toJSON value

parseInlineOptionsDatum :: Value -> OptionsDatum
parseInlineOptionsDatum value =
  fromMaybe (OptionsDatumError value) $ asum
    [ OptionsProposalDatum <$> decodeData @Options.ProposalDatum value
    , OptionsActiveDatum <$> decodeData @Options.ActiveDatum value
    ]

-------------------------------------------------
-- Options UTxO
-------------------------------------------------
-- | The type representing the information of a UTxO for an options address. 
data OptionsUTxO = OptionsUTxO
  { utxoRef :: TxOutRef
  , optionsAddress :: PaymentAddress
  , lovelace :: Lovelace
  , optionsDatum :: Maybe OptionsDatum
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsUTxO

instance Default OptionsUTxO where
  def = OptionsUTxO
    { utxoRef = TxOutRef "" 0
    , optionsAddress = ""
    , lovelace = 0
    , optionsDatum = Nothing
    , nativeAssets = []
    , blockTime = 0
    , blockHeight = 0
    }

instance ToJSON OptionsUTxO where
  toJSON OptionsUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "options_address" .= optionsAddress
           , "lovelace" .= lovelace
           , "options_datum" .= optionsDatum
           , "native_assets" .= nativeAssets
           , "block_time" .= blockTime
           , "block_height" .= blockHeight
           ]

instance FromJSON OptionsUTxO where
  parseJSON = withObject "OptionsUTxO" $ \o ->
    OptionsUTxO
      <$> o .: "utxo_ref"
      <*> o .: "options_address"
      <*> o .: "lovelace"
      <*> o .: "options_datum"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"

instance FromAddressUTxO OptionsUTxO where
  fromAddressUTxO AddressUTxO{..} = OptionsUTxO
      { utxoRef = utxoRef
      , optionsAddress = paymentAddress
      , lovelace = lovelace
      , optionsDatum = parseInlineOptionsDatum <$> inlineDatum
      , nativeAssets = nativeAssets
      , blockTime = blockTime
      , blockHeight = blockHeight
      }

-- | Get the ProposalDatum from a LoanUTxO.
optionsUTxOProposalDatum :: OptionsUTxO -> Maybe Options.ProposalDatum
optionsUTxOProposalDatum OptionsUTxO{optionsDatum} = case optionsDatum of
  Just (OptionsProposalDatum proposalDatum) -> Just proposalDatum
  _ -> Nothing

-- | Get the ActiveDatum from an OptionsUTxO.
optionsUTxOActiveDatum :: OptionsUTxO -> Maybe Options.ActiveDatum
optionsUTxOActiveDatum OptionsUTxO{optionsDatum} = case optionsDatum of
  Just (OptionsActiveDatum activeDatum) -> Just activeDatum
  _ -> Nothing

-- | Get the payment address from a OptionsUTxO.
optionsUTxOPaymentAddress :: OptionsUTxO -> Maybe Address
optionsUTxOPaymentAddress OptionsUTxO{optionsDatum} = case optionsDatum of
  Just (OptionsProposalDatum Options.ProposalDatum{paymentAddress}) -> Just paymentAddress
  Just (OptionsActiveDatum Options.ActiveDatum{paymentAddress}) -> Just paymentAddress
  _ -> Nothing

-- | Get the loan amount from a LoanUTxO. The loan principle is the quantity.
optionsUTxOOfferAmount :: OptionsUTxO -> Maybe NativeAsset
optionsUTxOOfferAmount OptionsUTxO{optionsDatum} = case optionsDatum of
  Just (OptionsProposalDatum Options.ProposalDatum{offerAsset,offerQuantity}) -> 
    Just $ toNativeAsset offerAsset & #quantity .~ offerQuantity
  Just (OptionsActiveDatum Options.ActiveDatum{offerAsset,offerQuantity}) -> 
    Just $ toNativeAsset offerAsset & #quantity .~ offerQuantity
  _ -> Nothing

contractHasExpired :: POSIXTime -> OptionsUTxO -> Bool
contractHasExpired currentTime OptionsUTxO{optionsDatum} = case optionsDatum of
  Just (OptionsActiveDatum Options.ActiveDatum{expiration}) -> 
    expiration <= toPlutusTime currentTime
  _ -> False

-------------------------------------------------
-- Options Wallet
-------------------------------------------------
-- | An options wallet is the CardanoOptions address using that staking credential. If the staking
-- credential is a paired hardware wallet, then `_stakeKeyPath` will be `Just derivationPath`. Only
-- loan wallets with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other options wallets,
-- like options loan wallets.
data OptionsWallet = OptionsWallet
  { network :: Network
  , profileId :: ProfileId
  -- | The wallet id used for the options wallet. 
  , optionsWalletId :: OptionsWalletId
  -- | The stake wallet id for the stake credential used.
  , stakeWalletId :: StakeWalletId
  -- | Alias for the stake credential. This will also be used as the alias for this options wallet.
  , alias :: Text
  , optionsAddress :: PaymentAddress
  , stakeAddress :: StakeAddress
  , stakeKeyDerivation :: Maybe DerivationInfo
  , stakeCredential :: Credential
  , utxos :: [OptionsUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  -- | The transaction history for this options address. 
  , transactions :: [Transaction]
  } deriving (Show,Eq,Generic,FromRow,ToRow)

makeFieldLabelsNoPrefix ''OptionsWallet

instance Ord OptionsWallet where
  p1 <= p2 = p1 ^. #optionsWalletId <= p2 ^. #optionsWalletId

instance Default OptionsWallet where
  def = OptionsWallet
    { network = def
    , profileId = 0
    , optionsWalletId = 0
    , stakeWalletId = 0
    , alias = "dummy" 
    , optionsAddress = "" 
    , stakeAddress = "" 
    , stakeKeyDerivation = Nothing 
    , stakeCredential = PubKeyCredential ""
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }

instance TableName OptionsWallet where
  tableName = "options_wallets"

instance Creatable OptionsWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE IF NOT EXISTS " <> tableName @OptionsWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "options_wallet_id INTEGER PRIMARY KEY"
        , "stake_wallet_id INTEGER REFERENCES stake_wallets (stake_wallet_id)"
        , "alias TEXT NOT NULL"
        , "options_address TEXT NOT NULL"
        , "stake_address TEXT NOT NULL"
        , "stake_key_derivation TEXT"
        , "stake_credential TEXT NOT NULL"
        , "utxos BLOB"
        , "lovelace INTEGER NOT NULL"
        , "native_assets BLOB"
        , "transactions BLOB"
        , "UNIQUE(network,profile_id,options_wallet_id,alias)"
        ]
    , ");"
    ]

instance Insertable OptionsWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @OptionsWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "options_wallet_id"
        , "stake_wallet_id"
        , "alias"
        , "options_address"
        , "stake_address"
        , "stake_key_derivation"
        , "stake_credential"
        , "utxos"
        , "lovelace"
        , "native_assets"
        , "transactions"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Notify OptionsWallet where
  notify currentTime oldState newState
    | msg /= [] =
        Just $ Notification
          { notificationType = OptionsNotification
          , alias = oldState ^. #alias
          , message = unlines msg
          , markedAsRead = False
          }
    | otherwise = Nothing
    where
      msg :: [Text]
      msg = filter (/= "")
        [ proposalsMsg
        , activesMsg
        ]

      proposalsOnly :: [OptionsUTxO] -> [TxOutRef]
      proposalsOnly = sort
                    . map (view #utxoRef) 
                    . filter (isJust . preview (#optionsDatum % _Just % _OptionsProposalDatum))

      activesOnly :: [OptionsUTxO] -> [TxOutRef]
      activesOnly = sort
                  . map (view #utxoRef) 
                  . filter (isJust . preview (#optionsDatum % _Just % _OptionsActiveDatum))

      proposalsMsg :: Text
      proposalsMsg
        | proposalsOnly (oldState ^. #utxos) /= proposalsOnly (newState ^. #utxos) = 
            "Proposed options contract statuses have changed."
        | otherwise = ""

      activesMsg :: Text
      activesMsg
        | activesOnly (oldState ^. #utxos) /= activesOnly (newState ^. #utxos) = 
            unwords $ intersperse "\n" $ filter (/="")
              [ "Active options contract statuses have changed."
              , if any (contractHasExpired currentTime) $ newState ^. #utxos
                then "Some contracts have expired"
                else ""
              ]
        | otherwise =
            if any (contractHasExpired currentTime) $ newState ^. #utxos
            then "Some contracts have expired"
            else ""

