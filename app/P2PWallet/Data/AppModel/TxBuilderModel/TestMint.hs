{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.TestMint where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Test Mint
-------------------------------------------------
-- | Information for minting test tokens for the user.
newtype TestMint = TestMint
  -- | The tokens to mint using the test token minting policy.
  { mint :: [TokenMint]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TestMint

instance Default TestMint where
  def = TestMint
    { mint = []
    }

instance AddToTxBody TestMint where
  addToTxBody txBody TestMint{..} = 
      txBody 
        -- Add the new mint. Order does not matter.
        & #mints %~ (newMint:)
    where 
      newMint :: TxBodyMint
      newMint = TxBodyMint
        { mintingPolicyHash = alwaysSucceedPolicyHash
        , nativeAssets = testMintToNativeAssets mint
        , redeemer = toRedeemer () -- Any redeemer will work.
        , scriptWitness = 
            -- The `alwaysSucceedPolicyScript` is small enough to be used directly without
            -- much of an impact.
            NormalWitness alwaysSucceedPolicyScript
        , executionBudget = def -- These must be calculated during the build step.
        }

testMintToNativeAssets :: [TokenMint] -> [NativeAsset]
testMintToNativeAssets = map $ \(TokenMint (name,num)) -> 
  let policyId = scriptHashToPolicyId alwaysSucceedPolicyHash in
    NativeAsset 
      { policyId = policyId
      , tokenName = name 
      , fingerprint = mkAssetFingerprint policyId name 
      , quantity = num
      }

-------------------------------------------------
-- New Test Mint
-------------------------------------------------
-- | Information from the user about which test tokens to mint.
data NewTestMint = NewTestMint
  -- | The tokens to mint using the test token minting policy. Each token must be separated
  -- by newlines.
  { mint :: Text
  -- | A user supplied text that they would like converted to hexidecimal.
  , exampleInput :: Text
  -- | The result of converting `exampleInput` to hexidecimal.
  , exampleOutput :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewTestMint

instance Default NewTestMint where
  def = NewTestMint
    { mint = ""
    , exampleInput = "TestToken1"
    , exampleOutput = "54657374546f6b656e31"
    }

-------------------------------------------------
-- NewTestMint <--> TestMint
-------------------------------------------------
-- | Verify the user info for the new test token mint.
processNewTestMint :: NewTestMint -> Either Text TestMint
processNewTestMint NewTestMint{..} = do
    verifiedMints <- sequence $ map parseLine $ lines mint

    let simplifiedMint = filter (\(TokenMint (_,num)) -> num /= 0) 
                       $ sumTokenMints verifiedMints

    when (null simplifiedMint) $ 
      Left "The net mint is zero so nothing will be added to the builder."

    return $ TestMint
      { mint = simplifiedMint
      }
  where
    parseLine :: Text -> Either Text TokenMint
    parseLine line = do
      res@(TokenMint (name,_)) <- maybeToRight ("Not a valid token mint: " <> line) $ parseTokenMint line

      when (length (toString $ display name) > 64) $ 
        Left $ "Hexidecimal token names must be <= 64 characters: " <> line

      return res

toNewTestMint :: TestMint -> NewTestMint
toNewTestMint TestMint{..} = NewTestMint
  { mint = unlines $ map display mint
  , exampleInput = ""
  , exampleOutput = ""
  }
