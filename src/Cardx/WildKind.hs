module Cardx.WildKind (WildKind (..)) where

import Data.Generics.Labels ()
import Relude

data WildKind = Wild | WildDraw4 deriving (Show, Eq, Generic)