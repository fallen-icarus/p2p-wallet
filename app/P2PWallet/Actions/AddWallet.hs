{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.AddWallet
  ( 
    pairPaymentWallet
  , pairStakeWallet
  , watchPaymentWallet
  , watchStakeWallet
  , processPaymentWallet
  , processStakeWallet
  ) where

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.DerivationPath
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Core.RegistrationStatus
import P2PWallet.Data.Lens hiding (network)
import P2PWallet.Data.Plutus
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Pairing Wallets
-------------------------------------------------
-- | Validate and then pair the new payment wallet.
pairPaymentWallet :: Network -> NewPaymentWallet -> IO PaymentWallet
pairPaymentWallet network NewPaymentWallet{..} = do
    when (_alias == "") $ throwIO $ AppError "Alias is empty."

    pKeyPath <- 
      fromJustOrAppError "Invalid payment key path." $ readDerivationPath _paymentKeyPath

    msKeyPath <- case fmap readDerivationPath _stakeKeyPath of
      Nothing -> return Nothing -- No stake key present.
      Just Nothing -> throwIO $ AppError "Invalid stake key path." -- Failed to read key path.
      Just res -> return res -- Successfully read key path.

    (payAddr,mStakeAddr) <- genAddresses pKeyPath msKeyPath

    return $ PaymentWallet 
      { _alias = _alias
      , _paymentAddress = payAddr 
      , _stakeAddress = mStakeAddr 
      , _paymentKeyPath = Just pKeyPath 
      , _stakeKeyPath = msKeyPath 
      , _utxos = [] 
      , _txHistory = [] 
      , _nativeAssets = [] 
      , _lovelaces = 0
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
pairStakeWallet :: Network -> NewStakeWallet -> IO StakeWallet
pairStakeWallet network NewStakeWallet{..} = do
    when (_alias == "") $ throwIO $ AppError "Alias is empty."

    sKeyPath <- 
      fromJustOrAppError "Invalid stake key path." $ readDerivationPath _stakeKeyPath

    stakeAddr <- genStakeAddress sKeyPath

    return $ StakeWallet 
      { _alias = _alias
      , _stakeAddress = stakeAddr 
      , _stakeKeyPath = Just sKeyPath 
      , _registrationStatus = NotRegistered
      , _totalDelegation = 0
      , _utxoBalance = 0
      , _availableRewards = 0
      , _delegatedPool = Nothing
      , _rewardHistory = [] 
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
watchPaymentWallet :: Network -> NewPaymentWallet -> IO PaymentWallet
watchPaymentWallet network NewPaymentWallet{..} = do
    when (_alias == "") $ throwIO $ AppError "Alias is empty."

    -- Check if the paymentAddress is a valid bech32 payment address. If it is, convert
    -- it to `PlutusAddress` and then back again since `plutusToBech32` will also generate
    -- the stake address, if any.
    (payAddr,mStakeAddr) <- fromRightOrAppError $ genAddresses _paymentAddress

    return $ PaymentWallet 
      { _alias = _alias
      , _paymentAddress = payAddr 
      , _stakeAddress = mStakeAddr 
      , _paymentKeyPath = Nothing
      , _stakeKeyPath = Nothing 
      , _utxos = [] 
      , _txHistory = [] 
      , _nativeAssets = [] 
      , _lovelaces = 0
      }

  where
    genAddresses :: Text -> Either Text (PaymentAddress, Maybe StakeAddress)
    genAddresses = 
      readPaymentAddress network >=> paymentAddressToPlutusAddress >=> plutusToBech32 network

-- | Validate and then watch the new wallet.
watchStakeWallet :: Network -> NewStakeWallet -> IO StakeWallet
watchStakeWallet network NewStakeWallet{..} = do
    when (_alias == "") $ throwIO $ AppError "Alias is empty."

    -- Check if the stakeAddress is a valid bech32 stake address. 
    stakeAddr <- fromRightOrAppError $ readStakeAddress network _stakeAddress

    return $ StakeWallet 
      { _alias = _alias
      , _stakeAddress = stakeAddr 
      , _stakeKeyPath = Nothing 
      , _registrationStatus = NotRegistered
      , _totalDelegation = 0
      , _utxoBalance = 0
      , _availableRewards = 0
      , _delegatedPool = Nothing
      , _rewardHistory = [] 
      }

-------------------------------------------------
-- Processing New Wallets
-------------------------------------------------
-- | Add the new payment wallet to the list of tracked payment wallets. If the new payment
-- wallet also has a staking credential, add the new staking credential to the tracked staking
-- wallets if it is not already present; the default alias name for the new `StakeWallet` is
-- the payment wallet alias suffixed with "_stake".
processPaymentWallet :: PaymentWallet -> Wallets -> Either Text Wallets 
processPaymentWallet newWallet@PaymentWallet{..} wallets' = do
    -- Check for clashes with other payment wallets.
    verifiyCanBeAdded

    return $ case _stakeAddress of
      -- If the payment wallet does not have staking, just add the new payment wallet.
      Nothing -> wallets' & paymentWallets .~ newWallets
      -- Check if the stake address is already being tracked.
      Just addr
        -- If the stake address is already tracked, just add the new payment wallet.
        | any (\acc -> addr == acc ^. stakeAddress) oldAccounts ->
            wallets' & paymentWallets .~ newWallets
        -- Otherwise also add the stake address to the tracked wallets.
        | otherwise ->
            let newAccount = def & alias .~ (newWallet ^. alias <> "_stake")
                                 & stakeAddress .~ addr
                                 & stakeKeyPath .~ _stakeKeyPath
            in wallets' & paymentWallets .~ newWallets
                        & stakeWallets .~ sortOn (view stakeAddress) (newAccount : oldAccounts)
  where 
    oldWallets :: [PaymentWallet]
    oldWallets = wallets' ^. paymentWallets

    verifiyCanBeAdded :: Either Text ()
    verifiyCanBeAdded
      | any ((==_alias) . view alias) oldWallets =
          Left "That alias is already being used by a payment wallet."
      | any ((==_paymentAddress) . view paymentAddress) oldWallets =
          Left "That payment address is already tracked under another alias."
      | otherwise = Right ()

    newWallets :: [PaymentWallet]
    newWallets = sortOn (view alias) $ newWallet : wallets' ^. paymentWallets

    oldAccounts :: [StakeWallet]
    oldAccounts = wallets' ^. stakeWallets

-- | Add the new stake wallet to the list of tracked stake wallets. 
processStakeWallet :: StakeWallet -> Wallets -> Either Text Wallets 
processStakeWallet newWallet@StakeWallet{..} wallets' = do
    -- Check for clashes with other payment wallets.
    verifiyCanBeAdded

    return $ wallets' & stakeWallets .~ sortOn (view stakeAddress) newWallets
  where 
    oldWallets :: [StakeWallet]
    oldWallets = wallets' ^. stakeWallets

    verifiyCanBeAdded :: Either Text ()
    verifiyCanBeAdded
      | any ((==_alias) . view alias) oldWallets =
          Left "That alias is already being used by a stake wallet."
      | any ((==_stakeAddress) . view stakeAddress) oldWallets =
          Left "That stake address is already tracked under another alias."
      | otherwise = Right ()

    newWallets :: [StakeWallet]
    newWallets = sortOn (view alias) $ newWallet : oldWallets
