module Cardx.GUI.Util (initialAppModel) where

import Cardx.GUI.Types (AppModel (AppModel), Scene (SPickDealer))
import Data.Default.Class qualified as D
import Relude (Bool (False), ($))
import System.Random qualified as R

initialAppModel :: AppModel
initialAppModel = AppModel D.def SPickDealer False $ R.mkStdGen 0