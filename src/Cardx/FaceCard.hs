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

module Cardx.FaceCard (FaceCard (..)) where

import Optics.TH (makeFieldLabelsNoPrefix)

data FaceCard = FaceCard
  {kind :: Integer, score :: Integer}
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''FaceCard