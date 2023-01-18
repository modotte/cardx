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

module Cardx.WildCard (WildCard (..)) where

import Cardx.WildKind (WildKind (..))
import Optics.TH (makeFieldLabelsNoPrefix)

data WildCard = WildCard
  { kind :: WildKind,
    score :: Integer
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''WildCard