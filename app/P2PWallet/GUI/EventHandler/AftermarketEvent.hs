module P2PWallet.GUI.EventHandler.AftermarketEvent
  ( 
    handleAftermarketEvent
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.EventHandler.AftermarketEvent.SellerEvent
import P2PWallet.Prelude

handleAftermarketEvent :: AppModel -> AftermarketEvent -> [AppEventResponse AppModel AppEvent]
handleAftermarketEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeAftermarketScene newScene -> 
    [ Model $ model & #aftermarketModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowAftermarketMorePopup -> 
    [ Model $ model & #aftermarketModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Add new Aftermarket wallet
  -----------------------------------------------
  AddNewAftermarketWallet modal -> case modal of
    StartAdding _ -> 
      let currentAftermarketWalletIds = map (view #stakeWalletId) $ knownWallets ^. #marketWallets
          availWallets = filter ((`notElem` currentAftermarketWalletIds) . view #stakeWalletId) $ 
            knownWallets ^. #stakeWallets
      in  [ Model $ model 
              & #aftermarketModel % #addingWallet .~ True -- Show widget.
              & #aftermarketModel % #targetStakeCredential .~ maybeHead availWallets
          ]
    CancelAdding -> 
      [ Model $ model 
          & #aftermarketModel % #addingWallet .~ False -- Hide widget.
          & #aftermarketModel % #targetStakeCredential .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (AftermarketEvent . AddNewAftermarketWallet . AddResult) $ do
          let stakeWallet = fromMaybe def $ aftermarketModel ^. #targetStakeCredential

          -- Get the new wallet id for the new entry into the market_wallet table.
          aftermarketWalletId <- getNextAftermarketWalletId databaseFile >>= fromRightOrAppError

          -- Create the new Aftermarket wallet.
          verifiedAftermarketWallet <- 
            fromRightOrAppError $ processNewAftermarketWallet aftermarketWalletId stakeWallet
          
          -- Add the new wallet to the database.
          insertAftermarketWallet databaseFile verifiedAftermarketWallet >>= fromRightOrAppError

          return verifiedAftermarketWallet
      ]
    AddResult verifiedAftermarketWallet ->
      [ Model $ model 
          & #knownWallets % #marketWallets %~ flip snoc verifiedAftermarketWallet
          & #aftermarketModel % #addingWallet .~ False
          & #aftermarketModel % #selectedWallet .~ verifiedAftermarketWallet
      , Task $ return $ SyncWallets $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Delete Aftermarket Wallet
  -----------------------------------------------
  DeleteAftermarketWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #aftermarketModel % #deletingWallet .~ True
                      & #aftermarketModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #aftermarketModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ AftermarketEvent $ DeleteAftermarketWallet PostDeletionAction) $ do
          -- Get the id for the wallet to delete.
          let currentId = aftermarketModel ^. #selectedWallet % #marketWalletId

          -- Delete the wallet.
          deleteAftermarketWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the wallet from the cached list of wallets.
      let currentId = aftermarketModel ^. #selectedWallet % #marketWalletId
          newWallets = filter (\w -> w ^. #marketWalletId /= currentId) $
            knownWallets ^. #marketWallets
       in [ Model $ model 
             & #aftermarketModel % #deletingWallet .~ False
             & #knownWallets % #marketWallets .~ newWallets
             & #aftermarketModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
          ]

  ---------------------------------------------
  -- Seller Event
  ---------------------------------------------
  AftermarketSellerEvent sellerEvt -> handleSellerEvent model sellerEvt

  -----------------------------------------------
  -- Lookup Key Info
  -----------------------------------------------
  LookupKeyInfo (forceResync,utxo) ->
    let (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts utxo in
    if policyId == Loans.activeBeaconCurrencySymbol then
      let synced tok = isJust $ Map.lookup (Loans.LoanId tok) (lendingModel ^. #cachedLoanHistories) in
      [ Event $ 
          if forceResync || not (all synced names) then
            LendingEvent $ LookupLoanHistories $ StartProcess $ Just $ map Loans.LoanId names
          else 
            AppInit
      ]
    else if policyId == Options.activeBeaconCurrencySymbol then
      let synced tok = isJust $ Map.lookup (Options.ContractId tok) (optionsModel ^. #cachedKeyContracts) in
      [ Event $ 
          if forceResync || not (all synced names) then
            OptionsEvent $ LookupOptionsContracts $ StartProcess $ Just $ map Options.ContractId names
          else
            AppInit
      ]
    else
      [ Event AppInit ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Process the new stake credential to generate the aftermarket wallet info. 
processNewAftermarketWallet :: MarketWalletId -> StakeWallet -> Either Text MarketWallet
processNewAftermarketWallet aftermarketWalletId StakeWallet{..} = do
  stakeCred <- stakeAddressToPlutusCredential stakeAddress
  marketAddress <- Aftermarket.genSellerAddress network $ Just stakeCred
  return $ MarketWallet
    { network = network
    , profileId = profileId
    , marketWalletId = aftermarketWalletId
    , stakeWalletId = stakeWalletId
    , alias = alias
    , marketAddress = marketAddress
    , stakeAddress = stakeAddress
    , stakeKeyDerivation = stakeKeyDerivation
    , stakeCredential = stakeCred
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    , bidUTxOs = []
    , bidTransactions = []
    }
