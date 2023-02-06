module Cardx.ActionKind (ActionKind (..)) where

import Relude
import TextShow (TextShow, showt)

data ActionKind = Skip | Draw2 deriving (Show, Eq, Generic)

instance TextShow ActionKind where
  showt :: ActionKind -> Text
  showt Skip = "Skip"
  showt Draw2 = "Draw2"
