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
    makeWilds,
    makeRange,
    makeColoredCardSet,
    makeColoreds,
    makeDeck,
    coloredScore,
    cardScore,
    pickDealer,
    nextTurn,
  )
where

import Cardx.ActionCard (ActionCard (..))
import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.FaceCard (FaceCard (..))
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind (..))
import Data.Vector (Vector, (!), (!?))
import Data.Vector qualified as V
import Relude
import Data.Generics.Labels ()

data GameProgression = Win | InProgress | Lose deriving (Show, Eq, Generic)

data Dealer = DPlayer | DComputer deriving (Show, Eq, Generic)

data Turn = GTPlayer | GTComputer deriving (Show, Eq, Generic)


data ColoredKind
  = CKActionCard ActionCard
  | CKFaceCard FaceCard
  deriving (Show, Eq, Generic)

data ColoredCard
  = RedCard ColoredKind
  | YellowCard ColoredKind
  | GreenCard ColoredKind
  | BlueCard ColoredKind
  deriving (Show, Eq, Generic)

data Card = CWild WildCard | CColored ColoredCard deriving (Show, Eq, Generic)

data GamePlayer = GamePlayer
  { hand :: Vector Card,
    score :: Natural,
    drawCount :: Natural
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

makeWilds :: WildKind -> Vector Card
makeWilds x = V.replicate 4 (CWild (WildCard x CC.wildScore))

makeRange :: Natural -> (Natural -> a) -> Vector a -> Vector a
makeRange from f xs =
  V.concat [xs, V.fromList $ map f [from .. 9]]

makeColoredCardSet :: Natural -> (ColoredKind -> ColoredCard) -> Vector ActionCard -> Vector ColoredCard
makeColoredCardSet from color =
  faces . actions
  where
    actions = V.map (color . CKActionCard)
    faces = makeRange from (\x -> color (CKFaceCard (FaceCard x x)))

makeColoreds :: Vector Card
makeColoreds =
  V.concatMap (\i -> V.concatMap (\x -> ofCColoredCards $ makeColoredCardSet i x acs) colors) (V.fromList [0, 1])
  where
    ofCColoredCards = V.map CColored
    colors = V.fromList [RedCard, YellowCard, GreenCard, BlueCard]
    acs =
      V.fromList
        [ ActionCard {kind = Skip, score = CC.actionScore},
          ActionCard {kind = Skip, score = CC.actionScore},
          ActionCard {kind = Draw2, score = CC.actionScore}
        ]

makeDeck :: Vector Card
makeDeck = V.concat [makeWilds Wild, makeWilds WildDraw4, makeColoreds]

coloredScore :: ColoredCard -> Natural
coloredScore =
  f . g
  where
    f (CKActionCard (ActionCard {kind = _, score = s})) = s
    f (CKFaceCard (FaceCard {kind = _, score = s})) = s
    g (RedCard c) = c
    g (YellowCard c) = c
    g (GreenCard c) = c
    g (BlueCard c) = c

cardScore :: Card -> Natural
cardScore (CWild (WildCard {kind = _, score = s})) = s
cardScore (CColored cc) = coloredScore cc

pickDealer :: Card -> Card -> Dealer
pickDealer pc cc = if cardScore pc > cardScore cc then DPlayer else DComputer

nextTurn :: Turn -> Turn
nextTurn GTComputer = GTPlayer
nextTurn GTPlayer = GTComputer
