module Cardx.GUI.Types (AppModel (..), Scene (..), AppEvent (..)) where

import Cardx.Model (Card, ColoredCard, GameState)
import Relude (Bool, Eq, Generic, Show)
import System.Random (StdGen)

data AppModel = AppModel
  { gameState :: GameState,
    currentScene :: Scene,
    hasPickedDealer :: Bool,
    oldRng :: StdGen
  }
  deriving (Show, Eq, Generic)

data Scene
  = SPickDealer
  | SPlay
  | SPickWildCardColor
  | SEndRound
  deriving (Show, Eq, Generic)

data AppEvent
  = AppIgnore
  | AppQuitGame
  | AppPickDealer
  | AppDealCards
  | AppClickDeckCard
  | AppClickHandCard Card
  | AppPickWildCardColor ColoredCard
  | AppResetRound
  | AppChangeScene Scene
  deriving (Show, Eq, Generic)