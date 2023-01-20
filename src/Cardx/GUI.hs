{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardx.GUI (launchGUI) where

import Cardx.Model
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Relude.Extra.Lens (over, (.~), (^.))
import TextShow qualified as TS

data AppModel = AppModel
  { clickCount :: Int
  }
  deriving (Show, Eq)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''AppModel

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
            [ label $ "Click count: " <> (TS.showt (1 :: Natural)),
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
  AppIncrease -> [Model model]

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