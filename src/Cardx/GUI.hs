module Cardx.GUI (launchGUI) where

import Cardx.GUI.Event qualified as CGE
import Cardx.GUI.Types
  ( AppEvent (AppIgnore),
    AppModel (gameState, oldRng),
  )
import Cardx.GUI.Util qualified as CGU
import Cardx.GUI.View qualified as CGV
import Cardx.Model (GameState (rng))
import Monomer
  ( appFontDef,
    appInitEvent,
    appTheme,
    appWindowTitle,
    darkTheme,
    startApp,
  )
import Optics ((%), (&), (.~))
import Relude (IO)
import System.Random qualified as R

launchGUI :: IO ()
launchGUI = do
  rng <- R.newStdGen
  let model = CGU.initialAppModel & #gameState % #rng .~ rng & #oldRng .~ rng
  startApp model CGE.handleEvent CGV.makeView config
  where
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppIgnore
      ]
