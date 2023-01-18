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
{-# LANGUAGE NoImplicitPrelude #-}

module Cardx.Model
  ( GameProgression (..),
    Dealer (..),
    Turn (..),
    ActionCard (..),
    WildCard (..),
    FaceCard (..),
    ColoredKind (..),
    ColoredCard (..),
    Card (..),
    GamePlayer (..),
    GameState (..),
  )
where

import Cardx.ActionCard (ActionCard (..))
import Cardx.FaceCard (FaceCard (..))
import Cardx.WildCard (WildCard (..))
import Data.Vector (Vector)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude (Eq, Integer, Maybe, Show)

data GameProgression = Win | InProgress | Lose deriving (Show, Eq)

makeFieldLabelsNoPrefix ''GameProgression

data Dealer = DPlayer | DComputer deriving (Show, Eq)

makeFieldLabelsNoPrefix ''Dealer

data Turn = GTPlayer | GTComputer deriving (Show, Eq)

makeFieldLabelsNoPrefix ''Turn

data ColoredKind
  = CKActionCard ActionCard
  | CKFaceCard FaceCard
  | Nothing
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''ColoredKind

data ColoredCard
  = RedCard ColoredKind
  | YellowCard ColoredKind
  | GreenCard ColoredKind
  | BlueCard ColoredKind
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''ColoredCard

data Card = CWild WildCard | CColoredCard ColoredCard deriving (Show, Eq)

makeFieldLabelsNoPrefix ''Card

data GamePlayer = GamePlayer
  { hand :: Vector Card,
    score :: Integer,
    drawCount :: Integer
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''GamePlayer

data GameState = GameState
  { player :: GamePlayer,
    computer :: GamePlayer,
    wildcardColor :: Maybe ColoredCard,
    deck :: Vector Card,
    drawPile :: Vector Card,
    turn :: Turn,
    dealer :: Dealer,
    progression :: GameProgression
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''GameState