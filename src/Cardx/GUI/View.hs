{-# LANGUAGE DataKinds #-}

module Cardx.GUI.View (makeView) where

import Cardx.Constant qualified as CC
import Cardx.GUI.Types
import Cardx.GUI.View.Internal (cardAsBtn, cardAsUnkBtn)
import Cardx.Model
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Relude hiding (id, (&))
import TextShow qualified as TS

makeView ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
makeView _ model = widgetTree
  where
    widgetTree =
      vstack
        [ case model.currentScene of
            SPickDealer -> pickDealerScene model
            SPlay -> playScene model
            SPickWildCardColor -> pickWildCardColorScene model
            SEndRound -> endScene model
        ]
        `styleBasic` [padding 10]

endScene ::
  ( TS.TextShow a1,
    TS.TextShow a2,
    TS.TextShow a3,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "player1" r3 r2,
    HasField "player2" r3 r1,
    HasField "progression" r3 GameProgression,
    HasField "turn" r3 a3,
    HasField "gameState" r4 r3
  ) =>
  r4 ->
  WidgetNode s AppEvent
endScene model =
  vstack
    [ label $ "To win the game score: " <> TS.showt CC.maxScore,
      label $ TS.showt TPlayer1 <> " score: " <> TS.showt model.gameState.player1.score,
      label $ TS.showt TPlayer2 <> " score: " <> TS.showt model.gameState.player2.score,
      label $ "Congratulations " <> TS.showt model.gameState.turn <> "!",
      label $ "You've " <> TS.showt model.gameState.progression,
      button "Quit?" AppQuitGame,
      if model.gameState.progression /= GPWin
        then button "Or continue on until someone wins the whole game?" AppResetRound
        else vstack []
    ]

pickDealerScene ::
  ( TS.TextShow a,
    HasField "turn" r1 a,
    HasField "gameState" r2 r1,
    HasField "hasPickedDealer" r2 Bool
  ) =>
  r2 ->
  WidgetNode s AppEvent
pickDealerScene model =
  vstack
    [ if model.hasPickedDealer
        then
          vstack
            [ label $ "Dealer of the round: " <> TS.showt model.gameState.turn,
              button "Play!" (AppChangeScene SPlay)
            ]
        else button "Pick a dealer" AppPickDealer
    ]

gameBoard ::
  ( HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "deck" r3 [Card],
    HasField "drawPile" r3 [Card],
    HasField "player1" r3 r2,
    HasField "player2" r3 r1,
    HasField "turn" r3 Turn,
    HasField "wildcardColor" r3 (Maybe ColoredCard)
  ) =>
  r3 ->
  WidgetNode s AppEvent
gameBoard gs =
  scroll $
    vstack
      [ label $ TS.showt TPlayer2,
        separatorLine,
        spacer,
        hstack $
          ( \x ->
              if gs.turn == TPlayer1
                then cardAsUnkBtn x AppIgnore
                else cardAsBtn Nothing x $ AppClickHandCard x
          )
            <$> V.toList gs.player2.hand,
        spacer,
        separatorLine,
        spacer,
        hstack
          [ case gs.deck of
              [] -> label "Empty deck!"
              (x : _) -> hstack [cardAsUnkBtn x AppClickDeckCard],
            spacer,
            separatorLine,
            spacer,
            case gs.drawPile of
              [] -> label "Empty draw pile!"
              (x : _) -> hstack [cardAsBtn gs.wildcardColor x AppIgnore]
          ],
        spacer,
        separatorLine,
        spacer,
        hstack $
          ( \x ->
              if gs.turn == TPlayer2
                then cardAsUnkBtn x AppIgnore
                else cardAsBtn Nothing x $ AppClickHandCard x
          )
            <$> V.toList gs.player1.hand,
        spacer,
        separatorLine,
        label $ TS.showt TPlayer1
      ]
      `styleBasic` [padding 10]

playScene ::
  ( HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "deck" r3 [Card],
    HasField "drawPile" r3 [Card],
    HasField "player1" r3 r2,
    HasField "player2" r3 r1,
    HasField "turn" r3 Turn,
    HasField "wildcardColor" r3 (Maybe ColoredCard),
    HasField "gameState" p r3
  ) =>
  p ->
  WidgetNode s AppEvent
playScene model =
  vstack
    [ label $ "Next turn: " <> (TS.showt . nextTurn) gs.turn,
      spacer,
      gameBoard gs
    ]
  where
    gs = model.gameState

pickWildCardColorScene ::
  ( HasField "gameState" p r,
    HasField "wildcardKind" r (Maybe a),
    TS.TextShow a
  ) =>
  p ->
  WidgetNode s AppEvent
pickWildCardColorScene model =
  vstack
    [ label $ "Pick " <> kt <> " color:",
      spacer,
      btn (AppPickWildCardColor $ RedCard ck) `styleBasic` [bgColor red],
      btn (AppPickWildCardColor $ YellowCard ck) `styleBasic` [bgColor yellow],
      btn (AppPickWildCardColor $ GreenCard ck) `styleBasic` [bgColor green],
      btn (AppPickWildCardColor $ BlueCard ck) `styleBasic` [bgColor blue]
    ]
  where
    kt = maybe "" TS.showt $ model.gameState.wildcardKind
    ck = CKFaceCard $ FaceCard {kind = 0, score = 1}
    btn = button ""
