{-# LANGUAGE DataKinds #-}

module Cardx.GUI.View (endScene, pickDealerScene, gameBoard) where

import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.GUI.Types
import Cardx.GUI.View.Internal (cardAsBtn, cardAsUnkBtn)
import Cardx.Model
import Cardx.Util qualified as CU
import Cardx.WildKind (WildKind (..))
import Data.Default.Class qualified as D
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Optics ((%), (&), (.~), (?~), (^.))
import Optics.Internal.Optic.Subtyping qualified
import Optics.Internal.Optic.Types qualified
import Optics.Label qualified
import Relude hiding (id, (&))
import System.Random qualified as R
import System.Random.Shuffle qualified as RS
import TextShow qualified as TS

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

gameBoard model =
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
  where
    gs = model.gameState