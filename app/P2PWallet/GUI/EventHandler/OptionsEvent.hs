module P2PWallet.GUI.EventHandler.OptionsEvent
  ( 
    handleOptionsEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.SyncOptions
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.EventHandler.OptionsEvent.BuyerEvent
import P2PWallet.GUI.EventHandler.OptionsEvent.WriterEvent
import P2PWallet.Prelude

handleOptionsEvent :: AppModel -> OptionsEvent -> [AppEventResponse AppModel AppEvent]
handleOptionsEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeOptionsScene newScene -> 
    [ Model $ model & #optionsModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowOptionsMorePopup -> 
    [ Model $ model & #optionsModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Add new Options wallet
  -----------------------------------------------
  AddNewOptionsWallet modal -> case modal of
    StartAdding _ -> 
      let currentOptionsWalletIds = map (view #stakeWalletId) $ knownWallets ^. #optionsWallets
          availWallets = filter ((`notElem` currentOptionsWalletIds) . view #stakeWalletId) $ 
            knownWallets ^. #stakeWallets
      in  [ Model $ model 
              & #optionsModel % #addingWallet .~ True -- Show widget.
              & #optionsModel % #targetStakeCredential .~ maybeHead availWallets
          ]
    CancelAdding -> 
      [ Model $ model 
          & #optionsModel % #addingWallet .~ False -- Show widget.
          & #optionsModel % #targetStakeCredential .~ Nothing
      ]
    ConfirmAdding -> 
      [ Task $ runActionOrAlert (OptionsEvent . AddNewOptionsWallet . AddResult) $ do
          let stakeWallet = fromMaybe def $ optionsModel ^. #targetStakeCredential

          -- Get the new wallet id for the new entry into the options_wallet table.
          optionsWalletId <- getNextOptionsWalletId databaseFile >>= fromRightOrAppError

          -- Create the new Options wallet.
          verifiedOptionsWallet <- 
            fromRightOrAppError $ processNewOptionsWallet optionsWalletId stakeWallet
          
          -- Add the new wallet to the database.
          insertOptionsWallet databaseFile verifiedOptionsWallet >>= fromRightOrAppError

          return verifiedOptionsWallet
      ]
    AddResult verifiedOptionsWallet ->
      [ Model $ model 
          & #knownWallets % #optionsWallets %~ flip snoc verifiedOptionsWallet
          & #optionsModel % #addingWallet .~ False
          & #optionsModel % #selectedWallet .~ verifiedOptionsWallet
      , Task $ return $ SyncWallets $ StartProcess Nothing
      ]

  -----------------------------------------------
  -- Delete Options Wallet
  -----------------------------------------------
  DeleteOptionsWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #optionsModel % #deletingWallet .~ True
                      & #optionsModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #optionsModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      -- Delete the wallet from the database.
      [ Task $ runActionOrAlert (const $ OptionsEvent $ DeleteOptionsWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = optionsModel ^. #selectedWallet % #optionsWalletId

          -- Delete the wallet.
          deleteOptionsWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Delete the wallet from the cached list of wallets.
      let currentId = optionsModel ^. #selectedWallet % #optionsWalletId
          newWallets = filter (\w -> w ^. #optionsWalletId /= currentId) $
            knownWallets ^. #optionsWallets
       in [ Model $ model 
             & #optionsModel % #deletingWallet .~ False
             & #knownWallets % #optionsWallets .~ newWallets
             & #optionsModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
          ]

  ---------------------------------------------
  -- Writer Event
  ---------------------------------------------
  OptionsWriterEvent writerEvt -> handleWriterEvent model writerEvt

  ---------------------------------------------
  -- Buyer Event
  ---------------------------------------------
  OptionsBuyerEvent buyerEvt -> handleBuyerEvent model buyerEvt

  -----------------------------------------------
  -- Sync Proposals
  -----------------------------------------------
  SyncOptionsProposals modal -> case modal of
    StartProcess mTargetAssets ->
      [ Model $ model & #waitingStatus % #syncingOptionsProposals .~ True
      , Task $ runActionOrAlert (OptionsEvent . SyncOptionsProposals . ProcessResults) $ do
          targetAssets <- 
            fromJustOrAppError "mTargetAssets is Nothing" mTargetAssets

          syncOptionsProposals (config ^. #network) targetAssets $ optionsModel ^. #cachedProposals
      ]
    ProcessResults newCache ->
      [ Model $ model 
          & #waitingStatus % #syncingOptionsProposals .~ False
          & #optionsModel % #cachedProposals .~ newCache
      ]

  -----------------------------------------------
  -- Sync Contracts for Key NFTs
  -----------------------------------------------
  LookupOptionsContract modal -> case modal of
    StartProcess mContractId ->
      [ Model $ model & #waitingStatus % #syncingOptionsContract .~ True
      , Task $ runActionOrAlert (OptionsEvent . LookupOptionsContract . ProcessResults) $ do
          contractId <- fromJustOrAppError "mContractId is Nothing" mContractId

          syncOptionsContract (config ^. #network) contractId $ optionsModel ^. #cachedKeyContracts
      ]
    ProcessResults newCache ->
      [ Model $ model 
          & #waitingStatus % #syncingOptionsContract .~ False
          & #optionsModel % #cachedKeyContracts .~ newCache
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Process the new stake credential to generate the options wallet info. 
processNewOptionsWallet :: OptionsWalletId -> StakeWallet -> Either Text OptionsWallet
processNewOptionsWallet optionsWalletId StakeWallet{..} = do
  stakeCred <- stakeAddressToPlutusCredential stakeAddress
  optionsAddress <- Options.genWriterAddress network $ Just stakeCred
  return $ OptionsWallet
    { network = network
    , profileId = profileId
    , optionsWalletId = optionsWalletId
    , stakeWalletId = stakeWalletId
    , alias = alias
    , optionsAddress = optionsAddress
    , stakeAddress = stakeAddress
    , stakeKeyDerivation = stakeKeyDerivation
    , stakeCredential = stakeCred
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }
