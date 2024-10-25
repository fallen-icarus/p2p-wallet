{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

A notification is triggered whenever the user's wallets change their state. It says what happened
and to what specific wallet. The notifications are _not_ backed up since it timestamping each
action can be quite computationally intensive. Instead, the notifications will just act as a
summary for the latest sync; newer syncs will overide the notifications from the prior sync.

-}
module P2PWallet.Data.Core.Internal.Notification where

import P2PWallet.Prelude

-------------------------------------------------
-- NotificationType
-------------------------------------------------
-- | Which category the notification pertains to.
data NotificationType
  = PaymentNotification
  | StakeNotification
  | DexNotification
  | LoanNotification
  | OptionsNotification
  | AftermarketNotification
  deriving (Show,Eq,Ord)

-------------------------------------------------
-- Notification
-------------------------------------------------
-- | A type represnting a notification.
data Notification = Notification
  -- | The notification category.
  { notificationType :: NotificationType
  -- | The alias of the wallet this notification is for.
  , alias :: Text
  -- | The actual notifcation.
  , message :: Text
  -- | Whether the notification has been marked as read.
  , markedAsRead :: Bool
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''Notification

instance Default Notification where
  def = Notification
    { notificationType = StakeNotification
    , alias = "Test"
    , message = "Stake credential registered and delegated to pool1111111199999999999999999999999.\n1.669 ADA was withdrawn from rewards."
    , markedAsRead = False
    }

-------------------------------------------------
-- Notify Class
-------------------------------------------------
class Notify a where
  -- | Generate the notification for a specific state change. If there is no state change,
  -- `notify` should return `Nothing` so it can be filtered out. The `POSIXTime` is the current
  -- time so that expirations can be checked.
  notify :: (oldState ~ a, newState ~ a) => POSIXTime -> oldState -> newState -> Maybe Notification
