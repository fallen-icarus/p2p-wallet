{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.MonomerOptics where

import Monomer
import Optics.TH

makeFieldLabels ''Color
makeFieldLabels ''WidgetEnv
makeFieldLabels ''WidgetNode
makeFieldLabels ''Theme
makeFieldLabels ''ThemeState
