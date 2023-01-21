module Cardx.GUI (launchGUI) where

import Cardx.Model
import Control.Lens
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import Relude hiding ((&))
import TextShow qualified as TS

newtype AppModel = AppModel
  { clickCount :: Int
  }
  deriving (Show, Eq, Generic)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Show, Eq)

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Hello world",
          spacer,
          hstack
            [ label $ "Click count: " <> TS.showt model.clickCount,
              spacer,
              button "Increase count" AppIncrease
            ]
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
  AppIncrease -> [Model (model & #clickCount .~ (model.clickCount + 1))]

launchGUI :: IO ()
launchGUI = do
  startApp model handleEvent buildUI config
  where
    model = AppModel 0
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]