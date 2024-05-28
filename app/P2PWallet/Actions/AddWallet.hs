{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.AddWallet
  ( 
    pairPaymentWallet
  , watchPaymentWallet
  ) where

import P2PWallet.Actions.ExportHwKey
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Profile
import P2PWallet.Data.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Pairing Wallets
-------------------------------------------------
-- | Validate the `NewPaymentWallet`, and export the keys from the hardware wallet.
pairPaymentWallet :: Network -> Profile -> PaymentId -> NewPaymentWallet -> IO PaymentWallet
pairPaymentWallet network Profile{profileId,accountIndex} paymentId NewPaymentWallet{..} = do
    when (alias == "") $ throwIO $ AppError "Wallet name is empty."

    pKeyPath <- 
      fromJustOrAppError "Invalid payment address index." $ 
        fmap (PaymentKeyPath accountIndex) $ toAddressIndex paymentAddressIndex

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

-------------------------------------------------
-- Watching Wallets
-------------------------------------------------
-- | Validate and then watch the new wallet.
watchPaymentWallet :: Network -> Profile -> PaymentId -> NewPaymentWallet -> IO PaymentWallet
watchPaymentWallet network Profile{profileId} paymentId NewPaymentWallet{..} = do
    when (alias == "") $ throwIO $ AppError "Alias is empty."

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
      }

  where
    genAddresses :: Text -> Either Text (PaymentAddress, Maybe StakeAddress)
    genAddresses = 
      readPaymentAddress network >=> paymentAddressToPlutusAddress >=> plutusToBech32 network
