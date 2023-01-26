module Cardx.Model
  ( GameProgression (..),
    Dealer (..),
    Turn (..),
    ActionCard (..),
    WildCard (..),
    FaceCard (..),
    ColoredKind (..),
    ColoredCard (..),
    CardKind (..),
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
    firstTurn,
    drawNFromDeck,
    eqColor,
    isMatchShape,
    getColoredKind,
    isEqualColoredKind,
  )
where

import Cardx.ActionCard (ActionCard (..))
import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.FaceCard (FaceCard (..))
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind (..))
import Data.Default.Class (Default (def))
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude
import TextShow (TextShow, showt)

data GameProgression = Win | InProgress | Lose deriving (Show, Eq, Generic)

data Dealer = DPlayer | DComputer deriving (Show, Eq, Generic)

data Turn = TPlayer | TComputer deriving (Show, Eq, Generic)

instance TextShow Turn where
  showt TPlayer = "Player"
  showt TComputer = "Computer"

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

data CardKind = CWild WildCard | CColored ColoredCard deriving (Show, Eq, Generic)

data Card = Card {id :: Natural, kind :: CardKind} deriving (Show, Eq, Generic)

data GamePlayer = GamePlayer
  { hand :: Vector Card,
    score :: Natural,
    drawCount :: Natural
  }
  deriving (Show, Eq, Generic)

instance Default GamePlayer where
  def = GamePlayer {hand = V.empty, score = 0, drawCount = 0}

data GameState = GameState
  { player :: GamePlayer,
    computer :: GamePlayer,
    wildcardColor :: Maybe ColoredCard,
    wildcardKind :: Maybe WildKind,
    deck :: [Card],
    drawPile :: [Card],
    turn :: Turn,
    dealer :: Dealer,
    progression :: GameProgression
  }
  deriving (Show, Eq, Generic)

instance Default GameState where
  def =
    GameState
      { player = def,
        computer = def,
        wildcardColor = Nothing,
        wildcardKind = Nothing,
        deck = makeDeck,
        drawPile = [],
        turn = TComputer,
        dealer = DComputer,
        progression = InProgress
      }

makeWilds :: WildKind -> Vector Card
makeWilds x = V.replicate 4 (Card {id = 0, kind = CWild (WildCard x CC.wildScore)})

makeRange :: Natural -> (Natural -> a) -> Vector a -> Vector a
makeRange from f xs = V.concat [xs, f <$> V.enumFromTo from 9]

makeColoredCardSet :: Natural -> (ColoredKind -> ColoredCard) -> Vector ActionCard -> Vector Card
makeColoredCardSet from color =
  faces . actions
  where
    actions = fmap (\x -> Card {id = 0, kind = CColored (color (CKActionCard x))})
    faces = makeRange from (\x -> Card {id = 0, kind = CColored (color (CKFaceCard (FaceCard x x)))})

makeColoreds :: Vector Card
makeColoreds =
  V.concatMap
    ( \i -> V.concatMap (\x -> makeColoredCardSet i x acs) colors
    )
    $ V.fromList [0, 1]
  where
    colors = V.fromList [RedCard, YellowCard, GreenCard, BlueCard]
    acs =
      V.fromList
        [ ActionCard {kind = Skip, score = CC.actionScore},
          ActionCard {kind = Skip, score = CC.actionScore},
          ActionCard {kind = Draw2, score = CC.actionScore}
        ]

makeDeck :: [Card]
makeDeck = V.toList xs
  where
    xs = V.imap (\i x -> Card {id = fromInteger . toInteger $ i, kind = x.kind}) cards
    cards = V.concat [makeWilds Wild, makeWilds WildDraw4, makeColoreds]

getColoredKind :: ColoredCard -> ColoredKind
getColoredKind (RedCard x) = x
getColoredKind (YellowCard x) = x
getColoredKind (GreenCard x) = x
getColoredKind (BlueCard x) = x

coloredScore :: ColoredCard -> Natural
coloredScore =
  f . getColoredKind
  where
    f (CKActionCard (ActionCard {score})) = score
    f (CKFaceCard (FaceCard {score})) = score

cardScore :: Card -> Natural
cardScore (Card {kind = CWild (WildCard {score})}) = score
cardScore (Card {kind = CColored cc}) = coloredScore cc

pickDealer :: Card -> Card -> Dealer
pickDealer pc cc = if cardScore pc > cardScore cc then DPlayer else DComputer

nextTurn :: Turn -> Turn
nextTurn TComputer = TPlayer
nextTurn TPlayer = TComputer

firstTurn :: Dealer -> Turn
firstTurn DComputer = TPlayer
firstTurn DPlayer = TComputer

type DeckToHand = Maybe ([Card], Vector Card)

drawOneAux :: DeckToHand -> DeckToHand
drawOneAux Nothing = Nothing
drawOneAux (Just ([], _)) = Nothing
drawOneAux (Just (x : xs, h)) = Just (xs, V.snoc h x)

drawOne :: State DeckToHand DeckToHand
drawOne = do
  s <- get
  put $ drawOneAux s
  pure s

drawNFromDeck :: Natural -> [State DeckToHand DeckToHand]
drawNFromDeck n = replicate (fromInteger . toInteger $ n) drawOne

eqColor :: ColoredCard -> ColoredCard -> Bool
(RedCard _) `eqColor` (RedCard _) = True
(YellowCard _) `eqColor` (YellowCard _) = True
(GreenCard _) `eqColor` (GreenCard _) = True
(BlueCard _) `eqColor` (BlueCard _) = True
_ `eqColor` _ = False

isEqualColoredKind :: ColoredCard -> ColoredCard -> Bool
isEqualColoredKind m n =
  let a = getColoredKind m
      b = getColoredKind n
   in case a of
        CKActionCard (ActionCard {kind = ka}) ->
          case b of
            CKActionCard (ActionCard {kind = kb}) ->
              ka == kb
            _ -> False
        CKFaceCard (FaceCard {kind = ka}) ->
          case b of
            CKFaceCard (FaceCard {kind = kb}) ->
              ka == kb
            _ -> False

isMatchShape :: Card -> Card -> Bool
isMatchShape
  Card {kind = card1}
  Card {kind = card2} =
    case card1 of
      CWild _ -> True
      CColored cc1 ->
        case card2 of
          CWild _ -> True
          CColored cc2 -> eqColor cc1 cc2 || isEqualColoredKind cc1 cc2
