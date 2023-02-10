{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardx.GUI.Event (handleEvent) where

import Cardx.GUI.Event.Internal qualified as CGEI
import Cardx.GUI.Types
import Cardx.GUI.Util qualified as CGU
import Cardx.Model
import Cardx.Util qualified as CU
import Cardx.WildKind (WildKind (..))
import Data.Vector ((!))
import Data.Vector qualified as V
import Monomer
import Optics ((%), (&), (.~), (?~), (^.))
import Relude hiding (id, (&))
import System.Random qualified as R

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt =
  let gs = model.gameState
   in case evt of
        AppIgnore -> []
        AppQuitGame -> [Request $ ExitApplication True]
        AppPickDealer ->
          [ Model $
              model
                & #hasPickedDealer .~ True
                & #gameState % #dealer .~ dealer
                & #gameState % #turn .~ firstTurn dealer
                & #gameState % #rng .~ old
                & #oldRng .~ new,
            Event AppDealCards
          ]
          where
            n = 1
            (old, new) = R.split model.oldRng
            (d, ph) = CGEI.unsafeF n (CGEI.shuffleCards gs.deck old, V.empty)
            (_, ch) = CGEI.unsafeF n (d, V.empty)
            dealer = pickDealer (ph ! 0) (ch ! 0)
        AppDealCards -> onDealCards model
        AppClickDeckCard ->
          [ Model $
              model
                & #gameState % CGEI.handFromTurn gs.turn % #hand .~ h
                & #gameState % #drawPile .~ fst piles
                & #gameState % #deck .~ snd piles
          ]
          where
            (d, h) =
              CGEI.unsafeF
                1
                ( gs.deck,
                  model ^. #gameState % CGEI.handFromTurn gs.turn % #hand
                )
            piles = CGEI.resetEmptyDeck gs.rng gs.drawPile d
        AppClickHandCard card@Card {id, kind} -> onClickHandCard card kind id model
        AppPickWildCardColor cc ->
          case gs.wildcardKind of
            Nothing -> []
            Just wck ->
              case wck of
                Wild -> [Model $ model' & CGEI.toNextTurn, Event $ AppChangeScene SPlay]
                WildDraw4 ->
                  [ Model $ CGEI.handleSpecialDrawCards model' 4 & CGEI.toNextTurn,
                    Event $ AppChangeScene SPlay
                  ]
          where
            model' = model & ((#gameState % #wildcardColor) ?~ cc)
        AppResetRound ->
          [ Model $
              CGU.initialAppModel
                & #oldRng .~ oldRng
                & #gameState % #player1 % #score .~ p1s
                & #gameState % #player2 % #score .~ p2s,
            Event $ AppChangeScene SPickDealer
          ]
          where
            oldRng = model.oldRng
            p1s = model.gameState.player1.score
            p2s = model.gameState.player2.score
        AppChangeScene scene ->
          let changeScene s = Model $ model & #currentScene .~ s
           in case scene of
                SPickDealer -> [changeScene SPickDealer]
                SPlay -> [changeScene SPlay]
                SPickWildCardColor -> [changeScene SPickWildCardColor]
                SEndRound -> [changeScene SEndRound]

onClickHandCard ::
  Card ->
  CardKind ->
  Natural ->
  AppModel ->
  [EventResponse AppModel AppEvent sp ep]
onClickHandCard selectedCard selectedCardKind selectedCardId model =
  let gs = model.gameState
      pileTopCard = CU.defaultIfEmpty selectedCard gs.drawPile
   in ( if isValidPattern selectedCard pileTopCard
          then
            ( let nh = V.filter (\c -> c.id /= selectedCardId) $ model ^. #gameState % CGEI.handFromTurn gs.turn % #hand
                  -- This cannot fail (player selection), so we default to the same card
                  ndp = selectedCard : gs.drawPile
                  hft m = CGEI.handFromTurn $ m ^. #gameState % #turn
                  model' =
                    model
                      & #gameState % hft model % #hand .~ nh
                      & #gameState % #drawPile .~ ndp
               in if V.null $ model' ^. #gameState % hft model' % #hand
                    then CGEI.handleRoundEnd model'
                    else case selectedCardKind of
                      CWild (WildCard {kind}) ->
                        [ Model $
                            model' & ((#gameState % #wildcardKind) ?~ kind),
                          Event $ AppChangeScene SPickWildCardColor
                        ]
                      CColored scc -> CGEI.handleColoredCards scc model'
            )
          else []
      )

onDealCards model =
  case f topCard of
    CWild (WildCard {kind}) ->
      [ Model $
          model' & ((#gameState % #wildcardKind) ?~ kind),
        Event $ AppChangeScene SPickWildCardColor
      ]
    CColored _ -> [Model model']
  where
    gs = model.gameState
    n = 7
    (d, ph) = CGEI.unsafeF n (CGEI.shuffleCards gs.deck gs.rng, V.empty)
    (d', ch) = CGEI.unsafeF n (d, V.empty)
    (d'', tc) = CGEI.unsafeF 1 (d', V.empty)
    topCard = tc ! 0
    f Card {kind} = kind
    model' =
      model
        & #gameState % #player1 % #hand .~ ph
        & #gameState % #player2 % #hand .~ ch
        & #gameState % #deck .~ d''
        & #gameState % #drawPile .~ [topCard]