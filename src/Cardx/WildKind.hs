module Cardx.WildKind (WildKind (..)) where

import Data.Generics.Labels ()
import Relude
import TextShow (TextShow, showt)

data WildKind = Wild | WildDraw4 deriving (Show, Eq, Generic)

instance TextShow WildKind where
  showt Wild = "Wild"
  showt WildDraw4 = "WildDraw4"