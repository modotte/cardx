{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardx.ActionKind (ActionKind (..)) where

import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

data ActionKind = Skip | Draw2 deriving (Show, Eq)

makeFieldLabelsNoPrefix ''ActionKind