{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardx.FaceCard (FaceCard (..)) where

import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

data FaceCard = FaceCard
  {kind :: Integer, score :: Integer}
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''FaceCard