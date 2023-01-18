{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardx.ActionCard (ActionCard (..)) where

import Cardx.ActionKind (ActionKind (..))
import Optics.TH (makeFieldLabelsNoPrefix)

data ActionCard = ActionCard
  { kind :: ActionKind,
    score :: Integer
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''ActionCard