module Cardx.ActionKind (ActionKind (..)) where

import Data.Generics.Labels ()
import Relude
import TextShow (TextShow, showt)

data ActionKind = Skip | Draw2 deriving (Show, Eq, Generic)

instance TextShow ActionKind where
  showt Skip = "Skip"
  showt Draw2 = "Draw"