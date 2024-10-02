module P2PWallet.Actions.AddWallet
  ( 
    pairPaymentWallet
  , pairStakeWallet
  , watchPaymentWallet
  , watchStakeWallet
  ) where

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Pairing Wallets
-------------------------------------------------
-- | Validate the `NewPaymentWallet`, and export the keys from the hardware wallet.
pairPaymentWallet 
  :: Network 
  -> Profile 
  -> PaymentWalletId 
  -> NewPaymentWallet 
  -> [PaymentWallet] -- ^ The currently tracked payment wallets.
  -> IO PaymentWallet
pairPaymentWallet network Profile{alias=_,network=_,..} paymentWalletId NewPaymentWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another payment wallet."

    pKeyInfo <- 
      fromJustOrAppError "Invalid payment address index." $ 
        (derivationType,) . PaymentKeyPath accountIndex <$> toAddressIndex paymentAddressIndex

    msKeyInfo <- case fmap toAddressIndex stakeAddressIndex of
      Nothing -> return Nothing -- No stake key present.
      Just Nothing -> throwIO $ AppError "Invalid stake address index."
      Just (Just addrIx) -> -- Successfully converted address index.
        return $ Just (derivationType, StakeKeyPath accountIndex addrIx)

    (payAddr,mStakeAddr) <- genAddresses pKeyInfo msKeyInfo

    return $ PaymentWallet 
      { network = network
      , profileId = profileId
      , paymentWalletId = paymentWalletId
      , alias = alias
      , paymentAddress = payAddr 
      , stakeAddress = mStakeAddr 
      , paymentKeyDerivation = Just pKeyInfo
      , stakeKeyDerivation = msKeyInfo
      , utxos = []
      , lovelace = 0
      , nativeAssets = []
      , transactions = []
      }

  where
    genAddresses 
      :: DerivationInfo
      -> Maybe DerivationInfo
      -> IO (PaymentAddress, Maybe StakeAddress)
    genAddresses paymentKeyInfo mStakingKeyInfo = do
      paymentKeyHash <- PubKeyCredential <$> exportHwPubKeyHash paymentKeyInfo
      mStakingKeyHash <- 
        maybe 
          (return Nothing) 
          (fmap (Just . StakingHash . PubKeyCredential) . exportHwPubKeyHash) 
          mStakingKeyInfo

      fromRightOrAppError $ plutusToBech32 network $ Address paymentKeyHash mStakingKeyHash

-- | Validate and then pair the new stake wallet.
pairStakeWallet 
  :: Network 
  -> Profile 
  -> StakeWalletId 
  -> NewStakeWallet 
  -> [StakeWallet] -- ^ Currently tracked stake wallets.
  -> IO StakeWallet
pairStakeWallet network Profile{alias=_,network=_,..} stakeWalletId NewStakeWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another stake wallet."

    sKeyInfo <- 
      fromJustOrAppError "Invalid stake address index." $ 
        (derivationType,) . StakeKeyPath accountIndex <$> toAddressIndex stakeAddressIndex

    stakeAddr <- genStakeAddress sKeyInfo

    return $ StakeWallet 
      { network = network
      , profileId = profileId
      , stakeWalletId = stakeWalletId
      , alias = alias
      , stakeAddress = stakeAddr 
      , stakeKeyDerivation = Just sKeyInfo
      , registrationStatus = NotRegistered
      , totalDelegation = 0
      , utxoBalance = 0
      , availableRewards = 0
      , delegatedPool = Nothing
      , rewardHistory = [] 
      , linkedAddresses = []
      }

  where
    genStakeAddress
      :: DerivationInfo 
      -> IO StakeAddress
    genStakeAddress stakeKeyInfo = do
      stakeKeyCred <- PubKeyCredential <$> exportHwPubKeyHash stakeKeyInfo

      -- Get the stake address. The payment credential does not matter so the stake key
      -- is used for both.
      (_,mStakeAddr) <- 
        fromRightOrAppError $ plutusToBech32 network $
          Address stakeKeyCred (Just $ StakingHash stakeKeyCred)

      fromJustOrAppError "Failed to generate a bech32 stake address" mStakeAddr

-------------------------------------------------
-- Watching Wallets
-------------------------------------------------
-- | Validate and then watch the new wallet.
watchPaymentWallet 
  :: Network 
  -> Profile 
  -> PaymentWalletId 
  -> NewPaymentWallet 
  -> [PaymentWallet] -- ^ Currently tracked payment wallets.
  -> IO PaymentWallet
watchPaymentWallet network Profile{profileId} paymentWalletId NewPaymentWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another payment wallet."

    -- Check if the paymentAddress is a valid bech32 payment address. If it is, convert
    -- it to `PlutusAddress` and then back again since `plutusToBech32` will also generate
    -- the stake address, if any.
    (payAddr,mStakeAddr) <- fromRightOrAppError $ genAddresses paymentAddress

    -- No script addresses are currently supported.
    fromRightOrAppError $ isPubKeyOnlyAddress $ toText payAddr

    -- No stake script addresses are currently supported.
    maybe (return ()) (fromRightOrAppError . isPubKeyOnlyAddress . toText) mStakeAddr

    return $ PaymentWallet 
      { network = network
      , profileId = profileId
      , paymentWalletId = paymentWalletId
      , alias = alias
      , paymentAddress = payAddr 
      , stakeAddress = mStakeAddr 
      , paymentKeyDerivation = Nothing
      , stakeKeyDerivation = Nothing 
      , utxos = [] 
      , lovelace = 0
      , nativeAssets = []
      , transactions = []
      }

  where
    genAddresses :: Text -> Either Text (PaymentAddress, Maybe StakeAddress)
    genAddresses = 
      parsePaymentAddress network >=> paymentAddressToPlutusAddress >=> plutusToBech32 network

-- | Validate and then watch the new wallet.
watchStakeWallet 
  :: Network 
  -> Profile 
  -> StakeWalletId 
  -> NewStakeWallet 
  -> [StakeWallet] -- ^ Currently tracked stake wallets.
  -> IO StakeWallet
watchStakeWallet network Profile{profileId} stakeWalletId NewStakeWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another stake wallet."

    -- Check if the stakeAddress is a valid bech32 stake address. 
    stakeAddr <- fromRightOrAppError $ parseStakeAddress network stakeAddress

    -- No script addresses are currently supported.
    fromRightOrAppError $ isPubKeyOnlyAddress $ toText stakeAddr

    return $ StakeWallet 
      { network = network
      , profileId = profileId
      , stakeWalletId = stakeWalletId
      , alias = alias
      , stakeAddress = stakeAddr 
      , stakeKeyDerivation = Nothing 
      , registrationStatus = NotRegistered
      , totalDelegation = 0
      , utxoBalance = 0
      , availableRewards = 0
      , delegatedPool = Nothing
      , rewardHistory = [] 
      , linkedAddresses = []
      }
