module Cardx.FaceCard (FaceCard (..)) where

import Relude

data FaceCard = FaceCard
  { kind :: Natural,
    score :: Natural
  }
  deriving (Show, Eq, Generic)
