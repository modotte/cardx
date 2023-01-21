module Cardx.ActionCard (ActionCard (..)) where

import Cardx.ActionKind (ActionKind (..))
import Data.Generics.Labels ()
import Relude

data ActionCard = ActionCard
  { kind :: ActionKind,
    score :: Natural
  }
  deriving (Show, Eq, Generic)