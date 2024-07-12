module P2PWallet.GUI.Widgets.Dex.Transactions 
  ( 
    transactionsWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget AppModel{dexModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      ]
  where
    DexWallet{..} = dexModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "No transactions found."
        `styleBasic` [textFont "Italics"]
