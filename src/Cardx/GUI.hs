{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardx.GUI (launchGUI) where

import Cardx.ActionKind (ActionKind (Skip))
import Cardx.Constant qualified as CC
import Cardx.Model
import Cardx.WildKind (WildKind (Wild))
import Control.Lens
import Data.Default.Class qualified as D
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?))
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Relude hiding ((&))
import TextShow qualified as TS

data AppModel = AppModel
  { gameState :: GameState,
    currentScene :: Scene,
    hasPickedDealer :: Bool
  }
  deriving (Show, Eq, Generic)

data Scene
  = SMenu
  | SPickDealer
  | SPlay
  | SEnd
  deriving (Show, Eq)

data AppEvent
  = AppInit
  | AppPickDealer
  | AppDealCards
  | AppClickCard Card
  | AppChangeScene Scene
  deriving (Show, Eq)

menuScene :: WidgetNode s AppEvent
menuScene =
  vstack
    [ button "Start" (AppChangeScene SPickDealer)
    ]

endScene :: WidgetNode s AppEvent
endScene =
  vstack
    [ label "You've TODO!!!",
      button "Go back to menu" (AppChangeScene SMenu),
      button "Or start over?" (AppChangeScene SPickDealer)
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
            [ label $ "Dealer of the game: " <> TS.showt model.gameState.turn,
              button "Play!" (AppChangeScene SPlay)
            ]
        else button "Pick a dealer" AppPickDealer
    ]

coloredCardAsButton :: Typeable e => ColoredKind -> e -> WidgetNode s e
coloredCardAsButton (CKActionCard (ActionCard {kind = k, score = s})) = button (TS.showt k)
coloredCardAsButton (CKFaceCard (FaceCard {kind = k, score = s})) = button (TS.showt k)

cardAsButton :: Card -> WidgetNode s AppEvent
cardAsButton card =
  let result evt =
        case card of
          Card {id = idx, kind = k} ->
            case k of
              CWild wk ->
                case wk of WildCard {kind = k, score = s} -> button (TS.showt k) evt `styleBasic` [textColor white, bgColor black]
              CColored cc ->
                case cc of
                  RedCard x -> coloredCardAsButton x evt
                  YellowCard x -> coloredCardAsButton x evt
                  GreenCard x -> coloredCardAsButton x evt
                  BlueCard x -> coloredCardAsButton x evt
   in result $ AppClickCard card

gameBoard ::
  ( HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "computer" r3 r2,
    HasField "player" r3 r1,
    HasField "gameState" r4 r3
  ) =>
  p ->
  r4 ->
  WidgetNode s AppEvent
gameBoard wenv model =
  scroll $
    vstack
      [ hstack $ fmap cardAsButton (V.toList model.gameState.computer.hand),
        spacer,
        spacer,
        hstack $ fmap cardAsButton (V.toList model.gameState.player.hand)
      ]

playScene ::
  ( TS.TextShow a1,
    TS.TextShow a2,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "hand" r2 (Vector Card),
    HasField "hand" r1 (Vector Card),
    HasField "computer" r3 r2,
    HasField "player" r3 r1,
    HasField "gameState" r4 r3
  ) =>
  p ->
  r4 ->
  WidgetNode s AppEvent
playScene wenv model =
  vstack
    [ label $ "Win score: " <> TS.showt CC.maxScore,
      label $ "Player score: " <> TS.showt model.gameState.player.score,
      label $ "Computer score: " <> TS.showt model.gameState.computer.score,
      spacer,
      gameBoard wenv model
    ]

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ case model.currentScene of
            SMenu -> menuScene
            SPickDealer -> pickDealerScene model
            SPlay -> playScene wenv model
            SEnd -> endScene
        ]
        `styleBasic` [padding 10]

initialModel :: AppModel
initialModel = AppModel D.def SMenu False

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppPickDealer ->
    [ Model $
        model
          & #hasPickedDealer .~ True
          & #gameState . #dealer .~ dealer
          & #gameState . #turn .~ firstTurn dealer,
      Event AppDealCards
    ]
    where
      -- TODO: Make sure to shuffle deck pre-and-post dealing, which in turn
      -- will randomize the dealer and hands.

      f = execState (sequence $ drawNFromDeck 1) . Just
      (xs, ph) = fromMaybe ([], V.empty) $ f (model.gameState.deck, V.empty)
      (_, ch) = fromMaybe ([], V.empty) $ f (xs, V.empty)
      dealer = pickDealer (ph ! 0) (ch ! 0)
  AppDealCards ->
    [ Model $
        model
          & #gameState . #player . #hand .~ ph
          & #gameState . #computer . #hand .~ ch
          & #gameState . #deck .~ xs'
    ]
    where
      f = execState (sequence $ drawNFromDeck 7) . Just
      (xs, ph) = fromMaybe ([], V.empty) $ f (model.gameState.deck, V.empty)
      (xs', ch) = fromMaybe ([], V.empty) $ f (xs, V.empty)
  AppClickCard c -> []
  AppChangeScene scene ->
    let changeScene s = Model $ model & #currentScene .~ s
     in case scene of
          SMenu -> [Model initialModel]
          SPickDealer -> [changeScene SPickDealer]
          SPlay -> [changeScene SPlay]
          SEnd -> [changeScene SEnd]

launchGUI :: IO ()
launchGUI = do
  startApp model handleEvent buildUI config
  where
    model = initialModel
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]