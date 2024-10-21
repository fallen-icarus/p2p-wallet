module P2PWallet.GUI.Widgets.TxBuilder.UserCertificates
  ( 
    userCertificatesList
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.TxBody
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.TxBuilder.Internal
import P2PWallet.Prelude

userCertificatesList :: [(Int,UserCertificate)] -> [AppNode]
userCertificatesList = map certificateRow
  where
    certificateRow :: (Int,UserCertificate) -> AppNode
    certificateRow (idx,UserCertificate{..}) = do
      let mainLabelCaption = fromString $ case certificateAction of
            Registration -> printf "Register %s" walletAlias
            Deregistration -> printf "Deregister %s" walletAlias
            Delegation _ -> printf "Delegate %s" walletAlias
      hstack
        [ vstack
            [ hstack
                [ label mainLabelCaption
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , widgetMaybe poolName $ \name ->
                    label name
                      `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ copyableLabelSelf (toText stakeAddress) lightGray 8
                , filler
                , widgetMaybe (certificateAction ^? _Delegation) $ \poolId ->
                    copyableLabelSelfWith 8 trimBech32 poolId lightGray
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserCertificate idx)
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , padding 3
                , radius 3
                , bgColor transparent
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        ]

