module Cardx.GUI (launchGUI) where

import Cardx.Constant qualified as CC
import Cardx.Model
import Control.Lens
import Data.Default.Class qualified as D
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import Relude hiding ((&))
import TextShow qualified as TS

data AppModel = AppModel
  { gameState :: GameState,
    clickCount :: Int
  }
  deriving (Show, Eq, Generic)

data Scene
  = SMenu
  | SPickDealer
  | SPlay
  | SEndGame
  deriving (Show, Eq)

data AppEvent
  = AppInit
  | AppScene Scene
  deriving (Show, Eq)

gameBoard wenv model =
  vstack
    [ hstack [],
      spacer,
      hstack [],
      spacer,
      hstack
        [ label "Pick a dealer",
          spacer,
          button "Proceed" (AppScene SPickDealer)
        ]
    ]

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label $ "Win score: " <> TS.showt CC.maxScore,
          label $ "Player score: " <> TS.showt model.gameState.player.score,
          label $ "Computer score: " <> TS.showt model.gameState.computer.score,
          spacer,
          gameBoard wenv model
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
  AppScene s -> case s of
    SPickDealer -> []

launchGUI :: IO ()
launchGUI = do
  startApp model handleEvent buildUI config
  where
    model = AppModel D.def 0
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]