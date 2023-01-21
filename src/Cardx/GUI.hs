{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardx.GUI (launchGUI) where

import Cardx.Constant qualified as CC
import Cardx.Model
import Control.Lens
import Data.Default.Class qualified as D
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Records (HasField)
import Monomer
import Relude hiding ((&))
import TextShow qualified as TS

data AppModel = AppModel
  { gameState :: GameState,
    currentScene :: Scene
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
  | AppChangeScene Scene
  deriving (Show, Eq)

menuScene :: WidgetNode s AppEvent
menuScene = button "Start game" (AppChangeScene SPickDealer)

endScene :: WidgetNode s AppEvent
endScene =
  vstack
    [ label "You've TODO!!!",
      button "Go back to menu" (AppChangeScene SMenu)
    ]

pickDealerScene =
  vstack
    [ button "Play!" (AppChangeScene SPlay)
    ]

gameBoard :: p1 -> p2 -> WidgetNode s AppEvent
gameBoard wenv model =
  vstack
    [ hstack [],
      spacer,
      hstack []
    ]

playScene ::
  ( TS.TextShow a1,
    TS.TextShow a2,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "computer" r3 r1,
    HasField "player" r3 r2,
    HasField "gameState" p2 r3
  ) =>
  p1 ->
  p2 ->
  WidgetNode s AppEvent
playScene wenv model =
  vstack
    [ label $ "Win score: " <> TS.showt CC.maxScore,
      label $ "Player score: " <> TS.showt (model.gameState.player.score),
      label $ "Computer score: " <> TS.showt (model.gameState.computer.score),
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
            SPickDealer -> pickDealerScene
            SPlay -> playScene wenv model
            SEnd -> endScene
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppChangeScene scene ->
    let changeScene s = model & #currentScene .~ s
     in case scene of
          SMenu -> [Model $ changeScene SMenu]
          -- TODO: Make sure to shuffle deck pre-and-post dealing.
          SPickDealer -> [Model $ changeScene SPickDealer]
          SPlay -> [Model $ changeScene SPlay]
          SEnd -> [Model $ changeScene SEnd]

launchGUI :: IO ()
launchGUI = do
  startApp model handleEvent buildUI config
  where
    model = AppModel D.def SMenu
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]