{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.LoanWallet where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import Database.SQLite.Simple (ToRow,FromRow)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.Transaction qualified as Koios
import P2PWallet.Database
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Loan Datum
-------------------------------------------------
-- | The loan datum used for the loan address UTxO.
data LoanDatum
  = AskDatum Loans.AskDatum
  | OfferDatum Loans.OfferDatum
  | ActiveDatum Loans.ActiveDatum
  -- | This is included in case a bug is found in the protocols.
  | LoanDatumError Value
  deriving (Eq,Show)

makePrisms ''LoanDatum

instance FromJSON LoanDatum where
  parseJSON value = pure $ fromMaybe (LoanDatumError value) $ asum
    [ AskDatum <$> parseMaybe (parseJSON @Loans.AskDatum) value
    , OfferDatum <$> parseMaybe (parseJSON @Loans.OfferDatum) value
    , ActiveDatum <$> parseMaybe (parseJSON @Loans.ActiveDatum) value
    ]

instance ToJSON LoanDatum where
  toJSON (AskDatum datum) = toJSON datum
  toJSON (OfferDatum datum) = toJSON datum
  toJSON (ActiveDatum datum) = toJSON datum
  toJSON (LoanDatumError value) = toJSON value

parseInlineLoanDatum :: Value -> LoanDatum
parseInlineLoanDatum value =
  fromMaybe (LoanDatumError value) $ asum
    [ AskDatum <$> decodeData @Loans.AskDatum value
    , OfferDatum <$> decodeData @Loans.OfferDatum value
    , ActiveDatum <$> decodeData @Loans.ActiveDatum value
    ]

-------------------------------------------------
-- Loan UTxO
-------------------------------------------------
-- | The type representing the information of a UTxO for a loan address. 
data LoanUTxO = LoanUTxO
  { utxoRef :: TxOutRef
  , loanAddress :: PaymentAddress
  , lovelace :: Lovelace
  , loanDatum :: Maybe LoanDatum
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''LoanUTxO

instance Default LoanUTxO where
  def = LoanUTxO
    { utxoRef = TxOutRef "" 0
    , loanAddress = ""
    , lovelace = 0
    , loanDatum = Nothing
    , nativeAssets = []
    , blockTime = 0
    , blockHeight = 0
    }

instance ToJSON LoanUTxO where
  toJSON LoanUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "loan_address" .= loanAddress
           , "lovelace" .= lovelace
           , "loan_datum" .= loanDatum
           , "native_assets" .= nativeAssets
           , "block_time" .= blockTime
           , "block_height" .= blockHeight
           ]

instance FromJSON LoanUTxO where
  parseJSON = withObject "LoanUTxO" $ \o ->
    LoanUTxO
      <$> o .: "utxo_ref"
      <*> o .: "loan_address"
      <*> o .: "lovelace"
      <*> o .: "loan_datum"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"

instance FromAddressUTxO LoanUTxO where
  fromAddressUTxO AddressUTxO{..} = LoanUTxO
      { utxoRef = utxoRef
      , loanAddress = paymentAddress
      , lovelace = lovelace
      , loanDatum = parseInlineLoanDatum <$> inlineDatum
      , nativeAssets = nativeAssets
      , blockTime = blockTime
      , blockHeight = blockHeight
      }

-- | Get the AskDatum from a LoanUTxO.
loanUTxOAskDatum :: LoanUTxO -> Maybe Loans.AskDatum
loanUTxOAskDatum LoanUTxO{loanDatum} = case loanDatum of
  Just (AskDatum askDatum) -> Just askDatum
  _ -> Nothing

-- | Get the OfferDatum from a LoanUTxO.
loanUTxOOfferDatum :: LoanUTxO -> Maybe Loans.OfferDatum
loanUTxOOfferDatum LoanUTxO{loanDatum} = case loanDatum of
  Just (OfferDatum offerDatum) -> Just offerDatum
  _ -> Nothing

-- | Get the ActiveDatum from a LoanUTxO.
loanUTxOActiveDatum :: LoanUTxO -> Maybe Loans.ActiveDatum
loanUTxOActiveDatum LoanUTxO{loanDatum} = case loanDatum of
  Just (ActiveDatum activeDatum) -> Just activeDatum
  _ -> Nothing

-- | Get the loan amount from a LoanUTxO. The loan principle is the quantity.
loanUTxOLoanAmount :: LoanUTxO -> Maybe NativeAsset
loanUTxOLoanAmount LoanUTxO{loanDatum} = case loanDatum of
  Just (AskDatum Loans.AskDatum{loanAsset,loanPrincipal}) -> 
    Just $ toNativeAsset loanAsset & #quantity .~ loanPrincipal
  Just (OfferDatum Loans.OfferDatum{loanAsset,loanPrincipal}) -> 
    Just $ toNativeAsset loanAsset & #quantity .~ loanPrincipal
  Just (ActiveDatum Loans.ActiveDatum{loanAsset,loanPrincipal}) -> 
    Just $ toNativeAsset loanAsset & #quantity .~ loanPrincipal
  _ -> Nothing

-- | Get the loan duration from a LoanUTxO.
loanUTxOLoanDuration :: LoanUTxO -> Maybe PlutusTime
loanUTxOLoanDuration LoanUTxO{loanDatum} = case loanDatum of
  Just (AskDatum Loans.AskDatum{loanTerm}) -> Just loanTerm
  Just (OfferDatum Loans.OfferDatum{loanTerm}) -> Just loanTerm
  Just (ActiveDatum Loans.ActiveDatum{loanTerm}) -> Just loanTerm
  _ -> Nothing

-- | Get the loan interest from a LoanUTxO.
loanUTxOLoanInterest :: LoanUTxO -> Maybe Loans.Fraction
loanUTxOLoanInterest LoanUTxO{loanDatum} = case loanDatum of
  Just (OfferDatum Loans.OfferDatum{loanInterest}) -> Just loanInterest
  Just (ActiveDatum Loans.ActiveDatum{loanInterest}) -> Just loanInterest
  _ -> Nothing

-- | Get the lender address from a LoanUTxO.
loanUTxOLenderAddress :: LoanUTxO -> Maybe Address
loanUTxOLenderAddress LoanUTxO{loanDatum} = case loanDatum of
  Just (OfferDatum Loans.OfferDatum{lenderAddress}) -> Just lenderAddress
  Just (ActiveDatum Loans.ActiveDatum{lenderAddress}) -> Just lenderAddress
  _ -> Nothing

-- | Get the next deadline from a LoanUTxO.
loanUTxONextDeadline :: LoanUTxO -> Maybe PlutusTime
loanUTxONextDeadline LoanUTxO{loanDatum} = case loanDatum of
  Just (ActiveDatum Loans.ActiveDatum{..}) -> 
    case (+lastCompounding) <$> compoundFrequency of
      Nothing -> Just loanExpiration
      Just nextCompounding -> Just $ min nextCompounding loanExpiration
  _ -> Nothing

-- | Get the next required payment from a LoanUTxO.
loanUTxORequiredPayment :: LoanUTxO -> Maybe NativeAsset
loanUTxORequiredPayment LoanUTxO{loanDatum} = case loanDatum of
  Just (ActiveDatum Loans.ActiveDatum{..}) -> 
    let amountDue = minPayment - totalEpochPayments in
      if minPayment == 0 then 
        Just $ toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
      else if amountDue < 0 then
        Just $ toNativeAsset loanAsset
      else
        Just $ toNativeAsset loanAsset & #quantity .~ amountDue
  _ -> Nothing

-- | Get the outstanding balance from a LoanUTxO.
loanUTxOLoanBalance :: LoanUTxO -> Maybe NativeAsset
loanUTxOLoanBalance LoanUTxO{loanDatum} = case loanDatum of
  Just (ActiveDatum Loans.ActiveDatum{loanAsset,loanOutstanding}) -> 
    Just $ toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
  _ -> Nothing

-------------------------------------------------
-- Loan Event
-------------------------------------------------
-- | The event for a particular loan.
data LoanEvent = LoanEvent
  -- | The loan this event is for.
  { loanId :: Loans.LoanId
  -- | The time of the event.
  , timeStamp :: POSIXTime
  -- | The state at the time of the event. This is the state _before_ the event occurs.
  -- When the loan is first started, the state will be `Nothing`.
  , state :: Maybe Loans.ActiveDatum
  -- | The action taken. Creating an active loan requires minting while all other actions involve
  -- spending the active loan input.
  , event :: Either Loans.ActiveBeaconsRedeemer Loans.LoanRedeemer
  } deriving (Show,Eq,Generic,FromJSON,ToJSON)

makeFieldLabelsNoPrefix ''LoanEvent

instance Default LoanEvent where
  def = LoanEvent
    { loanId = ""
    , timeStamp = 0
    , state = Nothing
    , event = Right Loans.AcceptOffer
    }

instance Ord LoanEvent where
  event1 <= event2
    | event1 ^. #loanId == event2 ^. #loanId = event1 ^. #timeStamp <= event2 ^. #timeStamp
    | otherwise = event1 ^. #loanId <= event2 ^. #loanId

toLoanEvent :: Loans.LoanId -> Koios.EventTransaction -> Maybe LoanEvent
toLoanEvent loanId Koios.EventTransaction{..} = do
    (parsedRedeemer, parsedDatum) <- 
      findLoanInput plutusContracts <|> findLoanStart plutusContracts

    return $ LoanEvent
      { loanId = loanId
      , state = parsedDatum
      , event = parsedRedeemer
      , timeStamp = blockTime
      }
  where
    -- This covers all but the start of the loan.
    findLoanInput 
      :: [Koios.TransactionPlutusContract] 
      -> Maybe (Either Loans.ActiveBeaconsRedeemer Loans.LoanRedeemer, Maybe Loans.ActiveDatum)
    findLoanInput [] = Nothing
    findLoanInput (Koios.TransactionPlutusContract{redeemer,datum}:xs) =
      case datum >>= decodeData @Loans.ActiveDatum of
        Nothing -> findLoanInput xs
        Just activeDatum -> 
          if activeDatum ^. #loanId == loanId 
          then (,Just activeDatum) . Right <$> decodeData redeemer
          else findLoanInput xs

    findLoanStart
      :: [Koios.TransactionPlutusContract]
      -> Maybe (Either Loans.ActiveBeaconsRedeemer Loans.LoanRedeemer, Maybe Loans.ActiveDatum)
    findLoanStart [] = Nothing
    findLoanStart (Koios.TransactionPlutusContract{redeemer}:xs) =
      case decodeData @Loans.ActiveBeaconsRedeemer redeemer of
        Just r@(Loans.CreateActive _) -> 
          let mintsLoanId NativeAsset{..} = and
                [ policyId == Loans.activeBeaconCurrencySymbol 
                , tokenName == Loans.unLoanId loanId 
                , quantity == 2
                ]
           in if any mintsLoanId mints
              then Just (Left r, Nothing)
              else findLoanStart xs
        _ -> findLoanStart xs

-------------------------------------------------
-- Loan Result
-------------------------------------------------
-- | The credit history for the borrower is a list of loan results.
data LoanResult = LoanResult
  -- | Whether the loan was defaulted on.
  { isDefault :: Bool
  -- | The remaining lovelace balance. This is only relevent for defaults.
  , remainingLovelace :: Lovelace
  -- | The remaining native assets. This is only relevent for defaults.
  , remainingNativeAssets :: [NativeAsset]
  -- | The loan terms.
  , terms :: Loans.ActiveDatum
  -- | The time of the final event.
  , completionTime :: POSIXTime
  } deriving (Show,Eq,Generic,FromJSON,ToJSON)

makeFieldLabelsNoPrefix ''LoanResult

instance Ord LoanResult where
  res1 <= res2 = res1 ^. #terms % #loanId <= res2 ^. #terms % #loanId

-- | Get all loan results from a transaction. One transaction can contain multiple results when
-- borrowers make final payments on multiple loans in the transaction.
toLoanResult :: Loans.BorrowerId -> Koios.EventTransaction -> [LoanResult]
toLoanResult borrowerId Koios.EventTransaction{..} = mapMaybe proccessEvent plutusContracts
  where
    proccessEvent :: Koios.TransactionPlutusContract -> Maybe LoanResult
    proccessEvent Koios.TransactionPlutusContract{..} = do
      parsedDatum@Loans.ActiveDatum{loanOutstanding} <- 
        datum >>= decodeData @Loans.ActiveDatum

      -- This execution must be for the specified borrower id. 
      when (parsedDatum ^. #borrowerId /= borrowerId) Nothing

      parsedRedeemer <- decodeData @Loans.LoanRedeemer redeemer
      Koios.TransactionUTxO{lovelace,nativeAssets} <- 
        find (\Koios.TransactionUTxO{inlineDatum} -> inlineDatum == datum) inputs
      case parsedRedeemer of
        Loans.MakePayment{paymentAmount} -> 
          if toRational paymentAmount < toRational loanOutstanding then Nothing else
            return $ LoanResult
              { isDefault = False
              , remainingLovelace = lovelace
              , remainingNativeAssets = nativeAssets
              , terms = parsedDatum
              , completionTime = blockTime
              }
        Loans.SpendWithKeyNFT -> 
          return $ LoanResult
            { isDefault = True
            , remainingLovelace = lovelace
            , remainingNativeAssets = nativeAssets
            , terms = parsedDatum
            , completionTime = blockTime
            }
        Loans.Unlock -> 
          return $ LoanResult
            { isDefault = True
            , remainingLovelace = lovelace
            , remainingNativeAssets = nativeAssets
            , terms = parsedDatum
            , completionTime = blockTime
            }
        _ -> Nothing
  
-------------------------------------------------
-- Loan Wallet
-------------------------------------------------
-- | A loan wallet is the CardanoLoans address using that staking credential. If the staking
-- credential is a paired hardware wallet, then `_stakeKeyPath` will be `Just derivationPath`. Only
-- loan wallets with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other loan wallets,
-- like cold loan wallets.
--
-- This wallet tracks all information for this staking credential when used as a borrower. However,
-- it only tracks offers (open and transaction history) when this staking credential is used as a
-- lender. The reason is that it becomes difficult to keep track of the associated Key NFTs since
-- they are paid to either a pubkey address or a proxy address. It is assumed that users will use
-- one of their tracked payment wallets for receiving the Key NFTs and future loan payments.
-- Therefore, the Home page is where users will go to see all of their current CardanoLoans Key NFTs
-- and the status of the associated loans.
data LoanWallet = LoanWallet
  { network :: Network
  , profileId :: ProfileId
  -- | The wallet id used for the loan wallet. 
  , loanWalletId :: LoanWalletId
  -- | The stake wallet id for the stake credential used.
  , stakeWalletId :: StakeWalletId
  -- | Alias for the stake credential. This will also be used as the alias for this loan wallet.
  , alias :: Text
  , loanAddress :: PaymentAddress
  , stakeAddress :: StakeAddress
  , stakeKeyDerivation :: Maybe DerivationInfo
  , stakeCredential :: Credential
  , utxos :: [LoanUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  -- | The transaction history for this loan address. This includes transactions initiated
  -- by lenders making/closing offers.
  , transactions :: [Transaction]
  -- | The wallet's credit history.
  , creditHistory :: [LoanResult]
  -- | Offer UTxOs for this staking credential used as a lender id. Only offer UTxOs are tracked.
  -- Once a borrower accepts the offer, the loan will not be tracked under the `LoanWallet`.
  -- Instead, lenders can keep track of active loans through their Key NFTs from the Home page.
  , offerUTxOs :: [LoanUTxO]
  -- | Transaction history of the Lender Id associated with this staking credential.
  , offerTransactions :: [Transaction]
  } deriving (Show,Eq,Generic,FromRow,ToRow)

makeFieldLabelsNoPrefix ''LoanWallet

instance Ord LoanWallet where
  p1 <= p2 = p1 ^. #loanWalletId <= p2 ^. #loanWalletId

instance Default LoanWallet where
  def = LoanWallet
    { network = def
    , profileId = 0
    , loanWalletId = 0
    , stakeWalletId = 0
    , alias = "dummy" 
    , loanAddress = "" 
    , stakeAddress = "" 
    , stakeKeyDerivation = Nothing 
    , stakeCredential = PubKeyCredential ""
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    , creditHistory = []
    , offerUTxOs = []
    , offerTransactions = []
    }

instance TableName LoanWallet where
  tableName = "loan_wallets"

instance Creatable LoanWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @LoanWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "loan_wallet_id INTEGER PRIMARY KEY"
        , "stake_wallet_id INTEGER REFERENCES stake_wallets (stake_wallet_id)"
        , "alias TEXT NOT NULL"
        , "loan_address TEXT NOT NULL"
        , "stake_address TEXT NOT NULL"
        , "stake_key_derivation TEXT"
        , "stake_credential TEXT NOT NULL"
        , "utxos BLOB"
        , "lovelace INTEGER NOT NULL"
        , "native_assets BLOB"
        , "transactions BLOB"
        , "credit_history BLOB"
        , "offer_utxos BLOB"
        , "offer_transactions BLOB"
        , "UNIQUE(network,profile_id,loan_wallet_id,alias)"
        ]
    , ");"
    ]

instance Insertable LoanWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @LoanWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "loan_wallet_id"
        , "stake_wallet_id"
        , "alias"
        , "loan_address"
        , "stake_address"
        , "stake_key_derivation"
        , "stake_credential"
        , "utxos"
        , "lovelace"
        , "native_assets"
        , "transactions"
        , "credit_history"
        , "offer_utxos"
        , "offer_transactions"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Notify LoanWallet where
  notify oldState newState
    | msg /= [] =
        Just $ Notification
          { notificationType = LoanNotification
          , alias = oldState ^. #alias
          , message = unlines msg
          , markedAsRead = False
          }
    | otherwise = Nothing
    where
      msg :: [Text]
      msg = filter (/= "")
        [ asksMsg
        , borrowerOffersMsg
        , lenderOffersMsg
        , activesMsg
        ]

      asksOnly :: [LoanUTxO] -> [TxOutRef]
      asksOnly = sort
               . map (view #utxoRef) . filter (isJust . preview (#loanDatum % _Just % _AskDatum))

      offersOnly :: [LoanUTxO] -> [TxOutRef]
      offersOnly = sort
                 . map (view #utxoRef) . filter (isJust . preview (#loanDatum % _Just % _OfferDatum))

      activeOnly :: [LoanUTxO] -> [TxOutRef]
      activeOnly = sort 
                 . map (view #utxoRef) . filter (isJust . preview (#loanDatum % _Just % _ActiveDatum))

      asksMsg :: Text
      asksMsg
        | asksOnly (oldState ^. #utxos) /= asksOnly (newState ^. #utxos) = 
            "Ask statuses have changed."
        | otherwise = ""

      borrowerOffersMsg :: Text
      borrowerOffersMsg
        | offersOnly (oldState ^. #utxos) /= offersOnly (newState ^. #utxos) = 
            "Offers from lenders have changed."
        | otherwise = ""

      activesMsg :: Text
      activesMsg
        | activeOnly (oldState ^. #utxos) /= activeOnly (newState ^. #utxos) = 
            "Active loan statuses have changed."
        | otherwise = ""

      lenderOffersMsg :: Text
      lenderOffersMsg
        | oldState ^. #offerUTxOs /= newState ^. #offerUTxOs = 
            "Offers to borrowers have changed."
        | otherwise = ""
