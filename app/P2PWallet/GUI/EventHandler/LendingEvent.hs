module P2PWallet.GUI.EventHandler.LendingEvent
  ( 
    handleLendingEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.SyncLoans
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.EventHandler.LendingEvent.BorrowEvent
import P2PWallet.GUI.EventHandler.LendingEvent.LendEvent
import P2PWallet.GUI.EventHandler.LendingEvent.ResearchEvent
import P2PWallet.Prelude

handleLendingEvent :: AppModel -> LendingEvent -> [AppEventResponse AppModel AppEvent]
handleLendingEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeLendingScene newScene -> 
    [ Model $ model & #lendingModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowLendingMorePopup -> 
    [ Model $ model & #lendingModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Add new loan wallet
  -----------------------------------------------
  AddNewLoanWallet modal -> case modal of
    StartAdding _ -> 
      let currentLoanWalletIds = map (view #stakeWalletId) $ knownWallets ^. #loanWallets
          availWallets = filter ((`notElem` currentLoanWalletIds) . view #stakeWalletId) $ 
            knownWallets ^. #stakeWallets
      in  [ Model $ model 
              & #lendingModel % #addingWallet .~ True -- Show widget.
              & #lendingModel % #targetStakeCredential .~ maybeHead availWallets
          ]
    CancelAdding -> 
      [ Model $ model 
          & #lendingModel % #addingWallet .~ False -- Show widget.
          & #lendingModel % #targetStakeCredential .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (LendingEvent . AddNewLoanWallet . AddResult) $ do
          let stakeWallet = fromMaybe def $ lendingModel ^. #targetStakeCredential

          -- Get the new wallet id for the new entry into the dex_wallet table.
          loanWalletId <- getNextLoanWalletId databaseFile >>= fromRightOrAppError

          -- Create the new Loan wallet.
          verifiedLoanWallet <- 
            fromRightOrAppError $ processNewLoanWallet loanWalletId stakeWallet
          
          -- Add the new payment wallet to the database.
          insertLoanWallet databaseFile verifiedLoanWallet >>= fromRightOrAppError

          return verifiedLoanWallet
      ]
    AddResult verifiedLoanWallet ->
      [ Model $ model 
          & #knownWallets % #loanWallets %~ flip snoc verifiedLoanWallet
          & #lendingModel % #addingWallet .~ False
          & #lendingModel % #selectedWallet .~ verifiedLoanWallet
      , Task $ return $ SyncWallets $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Delete Loan Wallet
  -----------------------------------------------
  DeleteLoanWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #lendingModel % #deletingWallet .~ True
                      & #lendingModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #lendingModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ LendingEvent $ DeleteLoanWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = lendingModel ^. #selectedWallet % #loanWalletId

          -- Delete the payment wallet.
          deleteLoanWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the payment wallet from the cached list of wallets.
      let currentId = lendingModel ^. #selectedWallet % #loanWalletId
          newWallets = filter (\w -> w ^. #loanWalletId /= currentId) $
            knownWallets ^. #loanWallets
       in [ Model $ model 
             & #lendingModel % #deletingWallet .~ False
             & #knownWallets % #loanWallets .~ newWallets
             & #lendingModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
          ]

  ---------------------------------------------
  -- Borrow Event
  ---------------------------------------------
  BorrowEvent borrowEvt -> handleBorrowEvent model borrowEvt

  ---------------------------------------------
  -- Lend Event
  ---------------------------------------------
  LendEvent lendEvt -> handleLendEvent model lendEvt

  ---------------------------------------------
  -- Research Event
  ---------------------------------------------
  LoanResearchEvent researchEvt -> handleResearchEvent model researchEvt

  -----------------------------------------------
  -- Sync Loan History
  -----------------------------------------------
  LookupLoanHistory modal -> case modal of
    StartProcess mLoanId ->
      [ Model $ model & #waitingStatus % #syncingLoanHistory .~ True
      , Task $ runActionOrAlert (LendingEvent . LookupLoanHistory . ProcessResults) $ do
          loanId <- fromJustOrAppError "mLoanId is Nothing"  mLoanId
          syncLoanHistory (config ^. #network) loanId $ lendingModel ^. #cachedLoanHistories
      ]
    ProcessResults newCachedLoanHistories ->
      [ Model $ model 
          & #waitingStatus % #syncingLoanHistory .~ False
          & #lendingModel % #cachedLoanHistories .~ newCachedLoanHistories
      ]

  -----------------------------------------------
  -- Sync Borrower Info
  -----------------------------------------------
  LookupBorrowerInformation modal -> case modal of
    StartProcess mInfo ->
      [ Model $ model & #waitingStatus % #syncingBorrowerInfo .~ True
      , Task $ runActionOrAlert (LendingEvent . LookupBorrowerInformation . ProcessResults) $ do
          (borrowerId, borrowerAddr) <- fromJustOrAppError "mInfo is Nothing"  mInfo
          syncBorrowerInfo (config ^. #network) borrowerId borrowerAddr $ 
            lendingModel ^. #cachedBorrowerInfo
      ]
    ProcessResults newCachedBorrowerInfo ->
      [ Model $ model 
          & #waitingStatus % #syncingBorrowerInfo .~ False
          & #lendingModel % #cachedBorrowerInfo .~ newCachedBorrowerInfo
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Process the new stake credential to generate the loan wallet info. 
processNewLoanWallet :: LoanWalletId -> StakeWallet -> Either Text LoanWallet
processNewLoanWallet loanWalletId StakeWallet{..} = do
  stakeCred <- stakeAddressToPlutusCredential stakeAddress
  loanAddress <- Loans.genLoanAddress network $ Just stakeCred
  return $ LoanWallet
    { network = network
    , profileId = profileId
    , loanWalletId = loanWalletId
    , stakeWalletId = stakeWalletId
    , alias = alias
    , loanAddress = loanAddress
    , stakeAddress = stakeAddress
    , stakeKeyDerivation = stakeKeyDerivation
    , stakeCredential = stakeCred
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    , creditHistory = []
    , offerUTxOs = []
    , offerTransactions = []
    }
