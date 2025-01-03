module P2PWallet.Actions.BalanceTx 
  ( 
    balanceTx
  ) where

import Data.List (foldl1')

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-- | Balance the inputs with the outputs by updating the changeOutput and subtracting off the
-- fee. This also accounts for registration deposits and reward withdrawals. This does not need
-- to account for beacon tokens since the management of those is automated.
balanceTx :: TxBuilderModel -> TxBuilderModel
balanceTx tx@TxBuilderModel{..} =
    tx & #changeOutput .~ 
          -- Deleting elements can result in `newChange` being `def` which should signal an empty
          -- builder.
          (if newChange == def then Nothing else Just newChange)
       & #isBalanced .~ balanced
       & #requiresCollateral .~ txNeedsCollateral
       & #txType .~ determinedTxType
       & #isBuilt .~ False
  where
    totalWithdrawn :: Lovelace
    totalWithdrawn = sum $ map (view #lovelace . snd) userWithdrawals

    testTokensMinted :: [NativeAsset]
    testTokensMinted = maybe [] (testMintToNativeAssets . view #mint) testMint

    -- The total deposit required from certificates.
    requiredDeposits :: Lovelace
    requiredDeposits = (flip . flip foldl') 0 userCertificates $ \acc (_,cert) ->
      case cert ^. #certificateAction of
        Registration -> acc + 2_000_000 -- 2 ADA must be paid.
        Deregistration -> acc - 2_000_000 -- 2 ADA must be returned.
        _ -> acc

    -- The amount of ADA and native assets from the input sources.
    inputValue :: (Lovelace,[NativeAsset])
    inputValue = sumAssetBalances
      [ assetBalancesForChange userInputs
      , (totalWithdrawn, [])
      , (0, testTokensMinted)
      , assetBalancesForChange $ swapBuilderModel ^. #swapCloses
      , assetBalancesForChange $ loanBuilderModel ^. #askCloses
      , assetBalancesForChange $ loanBuilderModel ^. #offerCloses
      , assetBalancesForChange $ optionsBuilderModel ^. #proposalCloses
      , assetBalancesForChange $ optionsBuilderModel ^. #expiredCloses
      , assetBalancesForChange $ aftermarketBuilderModel ^. #saleCloses
      , assetBalancesForChange $ aftermarketBuilderModel ^. #bidCloses
      , assetBalancesForChange $ aftermarketBuilderModel ^. #bidUnlocks
      ]

    -- The amount of ADA and native assets from the output sources. All quantities in this list must
    -- be negative so that they can be subtracted from the `inputValue`.
    outputValue :: (Lovelace,[NativeAsset])
    outputValue = sumAssetBalances
      [ assetBalancesForChange userOutputs
      , assetBalancesForChange $ swapBuilderModel ^. #swapCreations
      , assetBalancesForChange $ loanBuilderModel ^. #askCreations
      , assetBalancesForChange $ loanBuilderModel ^. #offerCreations
      , assetBalancesForChange $ optionsBuilderModel ^. #proposalCreations
      , assetBalancesForChange $ aftermarketBuilderModel ^. #saleCreations
      , assetBalancesForChange $ aftermarketBuilderModel ^. #bidCreations
      , (-requiredDeposits, [])
      , (-fee, [])
      ]

    (lovelaceChange, assetsChange) = sumAssetBalances 
      [ inputValue
      , outputValue
      -- These have inputs and outputs so they are sub-balances.
      , assetBalancesForChange $ swapBuilderModel ^. #swapUpdates
      , assetBalancesForChange $ swapBuilderModel ^. #swapExecutions
      , assetBalancesForChange $ loanBuilderModel ^. #askUpdates
      , assetBalancesForChange $ loanBuilderModel ^. #offerUpdates
      , assetBalancesForChange $ loanBuilderModel ^. #offerAcceptances
      , assetBalancesForChange $ loanBuilderModel ^. #loanPayments
      , assetBalancesForChange $ loanBuilderModel ^. #interestApplications
      , assetBalancesForChange $ loanBuilderModel ^. #expiredClaims
      , assetBalancesForChange $ loanBuilderModel ^. #keyBurns
      , assetBalancesForChange $ loanBuilderModel ^. #addressUpdates
      , assetBalancesForChange $ optionsBuilderModel ^. #proposalUpdates
      , assetBalancesForChange $ optionsBuilderModel ^. #proposalPurchases
      , assetBalancesForChange $ optionsBuilderModel ^. #addressUpdates
      , assetBalancesForChange $ optionsBuilderModel ^. #keyBurns
      , assetBalancesForChange $ optionsBuilderModel ^. #contractExecutions
      , assetBalancesForChange $ aftermarketBuilderModel ^. #saleUpdates
      , assetBalancesForChange $ aftermarketBuilderModel ^. #loanKeySpotPurchases
      , assetBalancesForChange $ aftermarketBuilderModel ^. #bidUpdates
      , assetBalancesForChange $ aftermarketBuilderModel ^. #claimBidAcceptances
      , assetBalancesForChange $ aftermarketBuilderModel ^. #loanKeyBidClaims
      , assetBalancesForChange $ aftermarketBuilderModel ^. #optionsKeySpotPurchases
      , assetBalancesForChange $ aftermarketBuilderModel ^. #spotBidAcceptances
      , assetBalancesForChange $ aftermarketBuilderModel ^. #optionsKeyBidClaims
      ]

    newChange :: ChangeOutput
    newChange = ChangeOutput
      { paymentAddress = fromMaybe "" $ changeOutput ^? _Just % #paymentAddress
      , lovelace = lovelaceChange
      , nativeAssets = assetsChange
      }

    -- Whether all assets are balanced.
    balanced :: Bool
    balanced = all ((>= 0) . view #quantity) assetsChange && lovelaceChange >= 0

    -- Whether this transaction requires collateral.
    txNeedsCollateral :: Bool
    txNeedsCollateral = or
      [ testTokensMinted /= []
      , swapBuilderModel ^. #swapCreations /= []
      , swapBuilderModel ^. #swapCloses /= []
      , swapBuilderModel ^. #swapUpdates /= []
      , swapBuilderModel ^. #swapExecutions /= []
      , loanBuilderModel ^. #askCreations /= []
      , loanBuilderModel ^. #askCloses /= []
      , loanBuilderModel ^. #askUpdates /= []
      , loanBuilderModel ^. #offerCreations /= []
      , loanBuilderModel ^. #offerCloses /= []
      , loanBuilderModel ^. #offerUpdates /= []
      , loanBuilderModel ^. #offerAcceptances /= []
      , loanBuilderModel ^. #loanPayments /= []
      , loanBuilderModel ^. #interestApplications /= []
      , loanBuilderModel ^. #expiredClaims /= []
      , loanBuilderModel ^. #keyBurns /= []
      , loanBuilderModel ^. #addressUpdates /= []
      , optionsBuilderModel ^. #proposalCreations /= []
      , optionsBuilderModel ^. #proposalCloses /= []
      , optionsBuilderModel ^. #proposalUpdates /= []
      , optionsBuilderModel ^. #proposalPurchases /= []
      , optionsBuilderModel ^. #expiredCloses /= []
      , optionsBuilderModel ^. #addressUpdates /= []
      , optionsBuilderModel ^. #keyBurns /= []
      , optionsBuilderModel ^. #contractExecutions /= []
      , aftermarketBuilderModel ^. #saleCreations /= []
      , aftermarketBuilderModel ^. #saleCloses /= []
      , aftermarketBuilderModel ^. #saleUpdates /= []
      , aftermarketBuilderModel ^. #loanKeySpotPurchases /= []
      , aftermarketBuilderModel ^. #bidCreations /= []
      , aftermarketBuilderModel ^. #bidCloses /= []
      , aftermarketBuilderModel ^. #bidUpdates /= []
      , aftermarketBuilderModel ^. #claimBidAcceptances /= []
      , aftermarketBuilderModel ^. #loanKeyBidClaims /= []
      , aftermarketBuilderModel ^. #optionsKeySpotPurchases /= []
      , aftermarketBuilderModel ^. #spotBidAcceptances /= []
      , aftermarketBuilderModel ^. #optionsKeyBidClaims /= []
      , aftermarketBuilderModel ^. #bidUnlocks /= []
      ]

    -- What kind of transaction this is.
    determinedTxType :: TxType
    determinedTxType
      | null knownKeyInfos = PairedTx
      | otherwise = foldl1' (<>) $ map (maybe WatchedTx $ const PairedTx) knownKeyInfos

    -- All required DerivationInfo for the transaction.
    knownKeyInfos :: [Maybe DerivationInfo]
    knownKeyInfos = concat
      [ map (view $ _2 % #stakeKeyDerivation) userWithdrawals
      , map (view $ _2 % #paymentKeyDerivation) userInputs
      , map (view $ _2 % #stakeKeyDerivation) userCertificates
      , maybe [] (pure . view #paymentKeyDerivation) collateralInput
      , map (view $ _2 % #stakeKeyDerivation) $ swapBuilderModel ^. #swapCloses
      , map (view $ _2 % #oldSwap % #stakeKeyDerivation) $ swapBuilderModel ^. #swapUpdates
      , map (view $ _2 % #borrowerKeyDerivation) $ loanBuilderModel ^. #askCreations
      , map (view $ _2 % #oldAsk % #stakeKeyDerivation) $ loanBuilderModel ^. #askUpdates
      , map (view $ _2 % #newAsk % #borrowerKeyDerivation) $ loanBuilderModel ^. #askUpdates
      , map (view $ _2 % #stakeKeyDerivation) $ loanBuilderModel ^. #askCloses
      , map (view $ _2 % #lenderKeyDerivation) $ loanBuilderModel ^. #offerCreations
      , map (view $ _2 % #oldOffer % #stakeKeyDerivation) $ loanBuilderModel ^. #offerUpdates
      , map (view $ _2 % #newOffer % #lenderKeyDerivation) $ loanBuilderModel ^. #offerUpdates
      , map (view $ _2 % #stakeKeyDerivation) $ loanBuilderModel ^. #offerCloses
      , map (view $ _2 % #stakeKeyDerivation) $ loanBuilderModel ^. #offerAcceptances
      , map (view $ _2 % #stakeKeyDerivation) $ loanBuilderModel ^. #loanPayments
      , map (view $ _2 % #stakeKeyDerivation) $ loanBuilderModel ^. #interestApplications
      , map (view $ _2 % #borrowerStakeKeyDerivation) $ loanBuilderModel ^. #expiredClaims
      , map (view $ _2 % #stakeKeyDerivation) $ optionsBuilderModel ^. #proposalCloses
      , map (view $ _2 % #oldProposal % #stakeKeyDerivation) $ optionsBuilderModel ^. #proposalUpdates
      , map (view $ _2 % #stakeKeyDerivation) $ optionsBuilderModel ^. #expiredCloses
      , map (view $ _2 % #stakeKeyDerivation) $ optionsBuilderModel ^. #addressUpdates
      , map (view $ _2 % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #saleCloses
      , map (view $ _2 % #oldSale % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #saleUpdates
      , map (view $ _2 % #marketWallet % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #bidCreations
      , map (view $ _2 % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #bidCloses
      , map (view $ _2 % #oldBid % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #bidUpdates
      , map (view $ _2 % #bidClaim % #marketWallet % #stakeKeyDerivation) $ 
          aftermarketBuilderModel ^. #loanKeyBidClaims
      , map (view $ _2 % #marketWallet % #stakeKeyDerivation) $ 
          aftermarketBuilderModel ^. #spotBidAcceptances
      , map (view $ _2 % #bidClaim % #marketWallet % #stakeKeyDerivation) $ 
          aftermarketBuilderModel ^. #optionsKeyBidClaims
      , map (view $ _2 % #sellerWallet % #stakeKeyDerivation) $ aftermarketBuilderModel ^. #bidUnlocks
      ]
