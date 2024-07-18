module P2PWallet.GUI.Widgets.Notifications
  ( 
    notificationsWidget
  ) where

import Monomer
import Data.List (groupBy)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal.Notification
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

notificationsWidget :: AppModel -> AppNode
notificationsWidget AppModel{notifications} = do
  centerWidget $ vstack
    [ centerWidgetH $ 
        hstack
          [ label "Notifications"
              `styleBasic` 
                [ paddingT 10
                , paddingB 10
                , textFont "Italics"
                , textColor white
                , textSize 18
                ]
          , spacer_ [width 5]
          , box_ [onClick MarkAllNotificationsAsRead] $
              tooltip_ "Mark all as read" [tooltipDelay 0] $
                label markReadIcon
                  `styleBasic`
                    [ bgColor black
                    , padding 5
                    , radius 5
                    , textColor customBlue
                    , textFont "Remix"
                    , textSize 12
                    , textMiddle
                    , textCenter
                    ]
                  `styleHover`
                    [ bgColor customGray3, cursorIcon CursorHand]
          ] `nodeVisible` (notifications /= [])
    , notificationsList notifications 
        `styleBasic` [padding 10, bgColor customGray2, radius 5]
        `nodeVisible` (notifications /= [])
    , noNotificationsMessage `nodeVisible` null notifications
    ] `styleBasic`
        [ padding 10
        , radius 5
        , bgColor transparent
        ]

noNotificationsMessage :: AppNode
noNotificationsMessage =
  centerWidgetH $ flip styleBasic [radius 5, padding 5, bgColor customGray2] $ box $
    label "Nothing to report."
      `styleBasic` [textFont "Italics"]

notificationsList :: [(Int,Notification)] -> AppNode
notificationsList = vstack . map noteSection . groupBy sameCategory . sortOn snd
  where
    sameCategory :: (Int,Notification) -> (Int,Notification) -> Bool
    sameCategory (_,note1) (_,note2) = note1 ^. #notificationType == note2 ^. #notificationType

    noteSection :: [(Int,Notification)] -> AppNode
    noteSection [] = spacer `nodeVisible` False
    noteSection xs@((_,Notification{notificationType}):_) = case notificationType of
      PaymentNotification -> specificNoteSection "Home" xs
      StakeNotification -> specificNoteSection "Staking" xs
      DexNotification -> specificNoteSection "Dex" xs

    noteRow :: (Int,Notification) -> AppNode
    noteRow (idx,Notification{..}) = do
      let (color, msgIcon, tip)
            | markedAsRead = (darkGray, markUnreadIcon, "Mark as unread")
            | otherwise = (white, markReadIcon, "Mark as read")
      hstack
        [ spacer
        , label ("For " <> alias <> ":")
            `styleBasic` [textSize 10, textColor color]
        , spacer_ [width 15]
        , label_ message [multiline]
            `styleBasic` [textSize 10, textColor color]
        , filler
        , box_ [onClick $ ToggleNotificationReadStatus idx] $
            tooltip_ tip [tooltipDelay 0] $
              label msgIcon
                `styleBasic`
                  [ bgColor black
                  , padding 5
                  , radius 5
                  , textColor customBlue
                  , textFont "Remix"
                  , textSize 12
                  , textMiddle
                  , textCenter
                  ]
                `styleHover`
                  [ bgColor customGray3, cursorIcon CursorHand]
        ] `styleBasic` [bgColor customGray4, padding 5, radius 5]

    specificNoteSection :: Text -> [(Int,Notification)] -> AppNode
    specificNoteSection caption xs = do
      vstack
        [ label caption
            `styleBasic` [textSize 12, textColor customBlue]
        , spacer
        , vstack_ [childSpacing] $ map noteRow xs
        ] `styleBasic` [padding 10, paddingB 0]
