module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder
  ( 
    aftermarketActionCount
  , saleCreationsList
  , editSaleCreationWidget
  , saleClosesList
  , saleUpdatesList
  , editSaleUpdateWidget
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCloses
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCreations
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleUpdates
import P2PWallet.Prelude

aftermarketActionCount :: AftermarketBuilderModel -> Int
aftermarketActionCount AftermarketBuilderModel{..} = sum
  [ length saleCreations
  , length saleCloses
  , length saleUpdates
  ]
