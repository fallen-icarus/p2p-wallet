{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module contains the primary key (aka, row id) types for the sqlite tables.

-}
module P2PWallet.Data.Core.Internal.PrimaryKeys where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import P2PWallet.Prelude

-- | The row id for the profiles sqlite table.
newtype ProfileId = ProfileId { unProfileId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''ProfileId

-- | The row id for the payment_wallets sqlite table.
newtype PaymentWalletId = PaymentWalletId { unPaymentWalletId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''PaymentWalletId

-- | The row id for the stake_wallets sqlite table.
newtype StakeWalletId = StakeWalletId { unStakeWalletId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''StakeWalletId

-- | The row id for the address_book sqlite table.
newtype ContactId = ContactId { unContactId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''ContactId

-- | The row id for the loan_wallets sqlite table.
newtype LoanWalletId = LoanWalletId { unLoanWalletId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''LoanWalletId

-- | The row id for the dex_wallets sqlite table.
newtype DexWalletId = DexWalletId { unDexWalletId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''DexWalletId
