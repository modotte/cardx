module Cardx.WildCard (WildCard (..)) where

import Cardx.WildKind (WildKind (..))
import Relude

data WildCard = WildCard
  { kind :: WildKind,
    score :: Natural
  }
  deriving (Show, Eq, Generic)
