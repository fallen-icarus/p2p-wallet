module P2PWallet.Actions.BalanceTx 
  ( 
    balanceTx
  ) where

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
        Delegation _ -> acc

    -- The amount of ADA and native assets from the input sources.
    inputValue :: (Lovelace,[NativeAsset])
    inputValue = sumAssetBalances
      [ assetBalancesForChange userInputs
      , (totalWithdrawn, [])
      , (0, testTokensMinted)
      , assetBalancesForChange $ swapBuilderModel ^. #swapCloses
      -- -- The output is already subtracted from the input.
      -- , assetBalancesForChange $ swapBuilderModel ^. #swapUpdates
      ]

    -- The amount of ADA and native assets from the output sources. All quantities in this list must
    -- be negative so that they can be subtracted from the `inputValue`.
    outputValue :: (Lovelace,[NativeAsset])
    outputValue = sumAssetBalances
      [ assetBalancesForChange userOutputs
      , assetBalancesForChange $ swapBuilderModel ^. #swapCreations
      , (requiredDeposits, [])
      , (-fee, [])
      ]

    (lovelaceChange, assetsChange) = sumAssetBalances 
      [ inputValue
      , outputValue
      , assetBalancesForChange $ swapBuilderModel ^. #swapUpdates
      , assetBalancesForChange $ swapBuilderModel ^. #swapExecutions
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
      ]
