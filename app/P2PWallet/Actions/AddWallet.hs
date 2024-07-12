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
  -> PaymentId 
  -> NewPaymentWallet 
  -> [PaymentWallet] -- ^ The currently tracked payment wallets.
  -> IO PaymentWallet
pairPaymentWallet network Profile{profileId,accountIndex} paymentId NewPaymentWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another payment wallet."

    pKeyPath <- 
      fromJustOrAppError "Invalid payment address index." $ 
        PaymentKeyPath accountIndex <$> toAddressIndex paymentAddressIndex

    msKeyPath <- case fmap toAddressIndex stakeAddressIndex of
      Nothing -> return Nothing -- No stake key present.
      Just Nothing -> throwIO $ AppError "Invalid stake address index."
      Just (Just addrIx) -> -- Successfully converted address index.
        return $ Just $ StakeKeyPath accountIndex addrIx

    (payAddr,mStakeAddr) <- genAddresses pKeyPath msKeyPath

    return $ PaymentWallet 
      { network = network
      , profileId = profileId
      , paymentId = paymentId
      , alias = alias
      , paymentAddress = payAddr 
      , stakeAddress = mStakeAddr 
      , paymentKeyPath = Just pKeyPath 
      , stakeKeyPath = msKeyPath 
      , utxos = []
      , lovelace = 0
      , nativeAssets = []
      , transactions = []
      }

  where
    genAddresses 
      :: DerivationPath 
      -> Maybe DerivationPath 
      -> IO (PaymentAddress, Maybe StakeAddress)
    genAddresses paymentKey mStakingKey = do
      paymentKeyHash <- PubKeyCredential <$> exportHwPubKeyHash paymentKey
      mStakingKeyHash <- 
        maybe 
          (return Nothing) 
          (fmap (Just . StakingHash . PubKeyCredential) . exportHwPubKeyHash) 
          mStakingKey

      fromRightOrAppError $ plutusToBech32 network $ Address paymentKeyHash mStakingKeyHash

-- | Validate and then pair the new stake wallet.
pairStakeWallet 
  :: Network 
  -> Profile 
  -> StakeId 
  -> NewStakeWallet 
  -> [StakeWallet] -- ^ Currently tracked stake wallets.
  -> IO StakeWallet
pairStakeWallet network Profile{profileId,accountIndex} stakeId NewStakeWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another stake wallet."

    sKeyPath <- 
      fromJustOrAppError "Invalid stake address index." $ 
        StakeKeyPath accountIndex <$> toAddressIndex stakeAddressIndex

    stakeAddr <- genStakeAddress sKeyPath

    return $ StakeWallet 
      { network = network
      , profileId = profileId
      , stakeId = stakeId
      , alias = alias
      , stakeAddress = stakeAddr 
      , stakeKeyPath = Just sKeyPath 
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
      :: DerivationPath 
      -> IO StakeAddress
    genStakeAddress stakeKey = do
      stakeKeyCred <- PubKeyCredential <$> exportHwPubKeyHash stakeKey

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
  -> PaymentId 
  -> NewPaymentWallet 
  -> [PaymentWallet] -- ^ Currently tracked payment wallets.
  -> IO PaymentWallet
watchPaymentWallet network Profile{profileId} paymentId NewPaymentWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another payment wallet."

    -- Check if the paymentAddress is a valid bech32 payment address. If it is, convert
    -- it to `PlutusAddress` and then back again since `plutusToBech32` will also generate
    -- the stake address, if any.
    (payAddr,mStakeAddr) <- fromRightOrAppError $ genAddresses paymentAddress

    return $ PaymentWallet 
      { network = network
      , profileId = profileId
      , paymentId = paymentId
      , alias = alias
      , paymentAddress = payAddr 
      , stakeAddress = mStakeAddr 
      , paymentKeyPath = Nothing
      , stakeKeyPath = Nothing 
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
  -> StakeId 
  -> NewStakeWallet 
  -> [StakeWallet] -- ^ Currently tracked stake wallets.
  -> IO StakeWallet
watchStakeWallet network Profile {profileId} stakeId NewStakeWallet{..} known = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    when (any ((== alias) . view #alias) known) $ 
      throwIO $ AppError "This name is already being used by another stake wallet."

    -- Check if the stakeAddress is a valid bech32 stake address. 
    stakeAddr <- fromRightOrAppError $ parseStakeAddress network stakeAddress

    return $ StakeWallet 
      { network = network
      , profileId = profileId
      , stakeId = stakeId
      , alias = alias
      , stakeAddress = stakeAddr 
      , stakeKeyPath = Nothing 
      , registrationStatus = NotRegistered
      , totalDelegation = 0
      , utxoBalance = 0
      , availableRewards = 0
      , delegatedPool = Nothing
      , rewardHistory = [] 
      , linkedAddresses = []
      }
