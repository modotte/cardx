{-# LANGUAGE DataKinds #-}

module Cardx.GUI.Event.Internal
  ( handFromTurn,
    handleColoredCards,
    handleSpecialDrawCards,
    handleRoundEnd,
    unsafeF,
    shuffleCards,
    resetEmptyDeck,
    toNextTurn,
  )
where

import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.GUI.Types
import Cardx.Model
import Data.Vector (Vector)
import Data.Vector qualified as V
import Monomer
import Optics ((%), (&), (.~), (^.))
import Relude hiding (id, (&))
import System.Random qualified as R
import System.Random.Shuffle qualified as RS

handFromTurn :: (IsLabel "player2" a, IsLabel "player1" a) => Turn -> a
handFromTurn TPlayer1 = #player1
handFromTurn TPlayer2 = #player2

unsafeF :: Natural -> ([Card], Vector Card) -> ([Card], Vector Card)
unsafeF n x =
  fromMaybe ([], V.empty) $
    (execState (sequence $ drawNFromDeck n) . Just) x

handleSpecialDrawCards :: AppModel -> Natural -> AppModel
handleSpecialDrawCards model n =
  model
    & #gameState % hft % #hand .~ h
    & #gameState % #deck .~ d
  where
    gs = model.gameState
    hft = handFromTurn $ nextTurn gs.turn
    (d, h) = unsafeF n (gs.deck, model ^. #gameState % hft % #hand)

shuffleCards :: R.RandomGen gen => [a] -> gen -> [a]
shuffleCards deck = RS.shuffle' deck (length deck)

resetEmptyDeck :: R.RandomGen gen => gen -> [a] -> [a] -> ([a], [a])
resetEmptyDeck rng drawPile [] =
  case drawPile of
    -- This shouldn't happen
    [] -> ([], [])
    (x : xs) ->
      ([x], shuffleCards xs rng)
resetEmptyDeck _ drawPile deck = (drawPile, deck)

sumRoundWinnerScore :: AppModel -> Natural
sumRoundWinnerScore model =
  s
  where
    other = handFromTurn $ nextTurn model.gameState.turn
    wh = model ^. #gameState % other % #hand
    s = V.foldl' (\a b -> cardScore b + a) 0 wh

toNextTurn :: AppModel -> AppModel
toNextTurn model = model & #gameState % #turn .~ nextTurn model.gameState.turn

updatedWildCardInfo ::
  AppModel ->
  ColoredCard ->
  Maybe AppModel
updatedWildCardInfo model scc =
  case model ^. #gameState % #wildcardColor of
    Nothing -> Just model
    Just wcc ->
      if eqColor scc wcc
        then
          model
            & #gameState % #wildcardColor .~ Nothing
            & #gameState % #wildcardKind .~ Nothing
            & Just
        else Nothing

handleColoredCards :: ColoredCard -> AppModel -> [EventResponse AppModel e sp ep]
handleColoredCards scc model =
  case getColoredKind scc of
    CKFaceCard _ ->
      maybe [] (\x -> [Model $ toNextTurn x]) $ updatedWildCardInfo model scc
    CKActionCard (ActionCard {kind}) ->
      case kind of
        Skip ->
          maybe [] (\x -> [Model x]) $ updatedWildCardInfo model scc
        Draw2 ->
          maybe
            []
            (\x -> [Model $ handleSpecialDrawCards x 2 & toNextTurn])
            $ updatedWildCardInfo model scc

handleRoundEnd :: AppModel -> [EventResponse AppModel AppEvent sp ep]
handleRoundEnd model =
  [ Model $
      model
        & #gameState % #progression
          .~ ( if ps >= CC.maxScore
                 then GPWin
                 else GPRound RPWin
             )
        & #gameState % hft % #score .~ ps,
    Event $ AppChangeScene SEndRound
  ]
  where
    hft = handFromTurn $ model ^. #gameState % #turn
    ps = sumRoundWinnerScore model