{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ProposalPurchase where

import Data.List ((!!))

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.Prelude

-------------------------------------------------
-- Proposal Purchase
-------------------------------------------------
-- | Information for purchasing an options contract.
data ProposalPurchase = ProposalPurchase
  -- | The proposal being purchased.
  { proposalUTxO :: OptionsUTxO
  -- | The index into the terms that is being purchased.
  , desiredTerms :: Integer
  -- | The extra minUTxOValue amount of ada required for the new active UTxO.
  , extraContractDeposit :: Lovelace
  -- | The minUTxOValue amount of ada for the premium payment output.
  , premiumDeposit :: Lovelace
  -- | Which network the contracts are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ProposalPurchase

instance AssetBalancesForChange (a,ProposalPurchase) where
  -- Must pay premium and cover any deposits.
  assetBalancesForChange xs =
    ( negate $ sum $ for xs $ \(_, ProposalPurchase{..}) ->
        let Options.ProposalDatum{premiumAsset,possibleTerms} = 
              fromMaybe def $ optionsUTxOProposalDatum proposalUTxO
            Options.Terms{premium} = possibleTerms !! fromIntegral desiredTerms
            NativeAsset{policyId} = toNativeAsset premiumAsset
         in if policyId == "" 
            then extraContractDeposit + premiumDeposit + Lovelace premium 
            else extraContractDeposit + premiumDeposit
    , sumNativeAssets $ concat $
        for xs $ \(_,ProposalPurchase{proposalUTxO=u@OptionsUTxO{utxoRef},..}) ->
          let Options.ProposalDatum{premiumAsset,possibleTerms} = 
                fromMaybe def $ optionsUTxOProposalDatum u
              Options.Terms{premium} = possibleTerms !! fromIntegral desiredTerms
              premiumNativeAsset@NativeAsset{policyId} = toNativeAsset premiumAsset
              contractId = 
                mkNativeAsset 
                  Options.activeBeaconCurrencySymbol 
                  (Options.unContractId $ Options.genContractId utxoRef)
           in [ contractId & #quantity .~ 1
              , if policyId == "" 
                then premiumNativeAsset & #quantity .~ 0 
                else premiumNativeAsset & #quantity .~ (-premium)
              ]
    )

optionsUTxOToProposalPurchase
  :: Network
  -> Integer
  -> OptionsUTxO
  -> ProposalPurchase
optionsUTxOToProposalPurchase network desiredTerms utxo = ProposalPurchase
  { proposalUTxO = utxo
  , desiredTerms = desiredTerms
  , extraContractDeposit = 0
  , premiumDeposit = 0
  , network = network
  }

-- | Update the required deposit increase if necessary.
updateOptionsPurchaseDeposits :: ProposalPurchase -> [Lovelace] -> Either Text ProposalPurchase
updateOptionsPurchaseDeposits p@ProposalPurchase{proposalUTxO,desiredTerms} calculatedDeposits =
    case calculatedDeposits of
      [premiumDeposit, contractDeposit] ->
        if premiumAsset ^. #unPremiumAsset % _1 == "" then
          -- The premium payment can cover the minUTxOValue if it is enough.
          return $ p
            & #premiumDeposit .~ max 0 (premiumDeposit - Lovelace premium)
            & #extraContractDeposit .~ max 0 (contractDeposit - lovelace)
        else
          -- The premium payment can't cover the deposit.
          return $ p
            & #premiumDeposit .~ premiumDeposit
            & #extraContractDeposit .~ max 0 (contractDeposit - lovelace)
      _ -> Left "calculateMinUTxOValue returned wrong results"
  where
    OptionsUTxO{lovelace} = proposalUTxO
    Options.ProposalDatum{contractDeposit=_,..} = fromMaybe def $ optionsUTxOProposalDatum proposalUTxO
    Options.Terms{premium} = possibleTerms !! fromIntegral desiredTerms

-- | Create the deposit message for the new options purchase.
createOptionsPurchaseDepositMsg :: ProposalPurchase -> Text
createOptionsPurchaseDepositMsg ProposalPurchase{..} = unlines $ intersperse "" $ filter (/= "")
    [ contractMsg
    , premiumMsg
    ]
  where
    contractMsg 
      | extraContractDeposit > 0 = unwords
          [ "The new contract UTxO requires an extra deposit of"
          , display extraContractDeposit
          , "in order to cover the minUTxOValue."
          , "You will need to provide the extra ada for this deposit if you wish to purchase this"
          , "contract."
          ]
      | otherwise = ""
    premiumMsg
      | premiumDeposit > 0 = unwords
          [ "The premium payment output requires an extra"
          , display premiumDeposit
          , "in order to cover the minUTxOValue."
          , "You will need to cover this amount in addition to the amount required for the premium."
          ]
      | otherwise = unwords
          [ "The premium covers the required minUTxOValue for the premium payment output."]
