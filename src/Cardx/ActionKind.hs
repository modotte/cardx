module Cardx.ActionKind (ActionKind (..)) where

import Data.Generics.Labels ()
import Relude

data ActionKind = Skip | Draw2 deriving (Show, Eq, Generic)