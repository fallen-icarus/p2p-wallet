{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.WriterAddressUpdate where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Address Update
-------------------------------------------------
-- | Information for updating a writer payment address.
data WriterAddressUpdate = WriterAddressUpdate
  { optionsUTxO :: OptionsUTxO
  -- | The stake credential for this writer.
  , writerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | The new payment address to use when the contract is executed.
  , newPaymentWallet :: PaymentWallet
  -- | The extra deposit required for the new contract UTxO.
  , extraDeposit :: Lovelace
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''WriterAddressUpdate

instance AssetBalancesForChange (a,WriterAddressUpdate) where
  assetBalancesForChange xs =
    -- Only the balance of ada can change and it can only increase by the extra amount required
    -- for the new minUTxOValue.
    ( sum $ map (negate . view (_2 % #extraDeposit)) xs
    , []
    )

-------------------------------------------------
-- New Address Update
-------------------------------------------------
-- | Information for updating a loan payment address.
data NewWriterAddressUpdate = NewWriterAddressUpdate
  { optionsUTxO :: OptionsUTxO
  -- | The new payment address.
  , newPaymentWallet :: PaymentWallet
  -- | The stake credential for this writer.
  , writerCredential :: Credential
  -- | The path to the required hw key for witnessing.
  , stakeKeyDerivation :: Maybe DerivationInfo
  -- | Which network the swaps are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Wallet this UTxO is from.
  , walletAlias :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewWriterAddressUpdate

instance Default NewWriterAddressUpdate where
  def = NewWriterAddressUpdate
    { optionsUTxO = def
    , newPaymentWallet = def
    , writerCredential = PubKeyCredential ""
    , stakeKeyDerivation = Nothing
    , network = def
    , walletAlias = ""
    }

-- | Create a fresh `NewWriterAddressUpdate`.
createNewWriterAddressUpdate 
  :: Network 
  -> Text 
  -> Credential 
  -> Maybe DerivationInfo 
  -> PaymentWallet -- ^ The new payment wallet to use.
  -> OptionsUTxO 
  -> NewWriterAddressUpdate
createNewWriterAddressUpdate network alias stakeCredential mKeyInfo newPaymentWallet optionsUTxO =
  NewWriterAddressUpdate
    { optionsUTxO = optionsUTxO
    , network = network
    , writerCredential = stakeCredential
    , stakeKeyDerivation = mKeyInfo
    , walletAlias = alias
    , newPaymentWallet = newPaymentWallet
    -- , newPaymentAddress = either (const "") (display . fst) 
    --                     $ plutusToBech32 network 
    --                     $ view #paymentAddress
    --                     $ fromMaybe def
    --                     $ optionsUTxOActiveDatum optionsUTxO
    }

-------------------------------------------------
-- NewWriterAddressUpdate <--> WriterAddressUpdate
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewWriterAddressUpdate :: POSIXTime -> NewWriterAddressUpdate -> Either Text WriterAddressUpdate
verifyNewWriterAddressUpdate currentTime NewWriterAddressUpdate{..} = do
  verifiedAddress <- parsePaymentAddress network $ display $ newPaymentWallet ^. #paymentAddress

  addrAsPlutus <- paymentAddressToPlutusAddress verifiedAddress

  unless (Options.isValidOptionsPaymentAddress addrAsPlutus) $
    Left $ unwords
      [ "Options payment addresses must either use a payment pubkey, or the proxy script as the"
      , "payment credential and a staking credential."
      ]

  return $ WriterAddressUpdate
    { optionsUTxO = optionsUTxO
    , network = network
    , newPaymentWallet = newPaymentWallet
    , extraDeposit = 0 -- this will be set later.
    , writerCredential = writerCredential
    , stakeKeyDerivation = stakeKeyDerivation
    , walletAlias = walletAlias
    , currentTime = toPlutusTime currentTime
    }

toNewWriterAddressUpdate :: WriterAddressUpdate -> NewWriterAddressUpdate
toNewWriterAddressUpdate WriterAddressUpdate{..} = NewWriterAddressUpdate
  { optionsUTxO = optionsUTxO
  , network = network
  , newPaymentWallet = newPaymentWallet
  , writerCredential = writerCredential
  , stakeKeyDerivation = stakeKeyDerivation
  , walletAlias = walletAlias
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateWriterAddressDeposit :: WriterAddressUpdate -> Lovelace -> WriterAddressUpdate
updateWriterAddressDeposit i@WriterAddressUpdate{optionsUTxO=OptionsUTxO{lovelace}} calculatedDeposit
  | calculatedDeposit > lovelace = i & #extraDeposit .~ calculatedDeposit - lovelace
  | otherwise = i & #extraDeposit .~ 0

-- | Generate the deposit message.
createWriterAddressDepositMsg :: WriterAddressUpdate -> Text
createWriterAddressDepositMsg WriterAddressUpdate{extraDeposit}
  | extraDeposit <= 0 = ""
  | otherwise = unwords
      [ "The new contract UTxO requires an extra"
      , display extraDeposit
      , "for the deposit."
      ]
