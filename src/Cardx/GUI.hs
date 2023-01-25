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

cardTextColor :: [StyleState]
cardTextColor = [textColor white]

wildCardAsButton :: Typeable e => WildCard -> e -> WidgetNode s e
wildCardAsButton (WildCard {kind = k, score = s}) evt =
  button (TS.showt k) evt `styleBasic` [bgColor black] <> cardTextColor

coloredKindAsButton :: Typeable e => ColoredKind -> e -> WidgetNode s e
coloredKindAsButton (CKActionCard (ActionCard {kind = k, score = s})) = button (TS.showt k)
coloredKindAsButton (CKFaceCard (FaceCard {kind = k, score = s})) = button (TS.showt k)

coloredCardAsButton :: Typeable e => ColoredCard -> e -> WidgetNode s e
coloredCardAsButton (RedCard x) evt =
  coloredKindAsButton x evt `styleBasic` [bgColor red] <> cardTextColor
coloredCardAsButton (YellowCard x) evt =
  coloredKindAsButton x evt `styleBasic` [bgColor yellow] <> cardTextColor
coloredCardAsButton (GreenCard x) evt =
  coloredKindAsButton x evt `styleBasic` [bgColor green] <> cardTextColor
coloredCardAsButton (BlueCard x) evt =
  coloredKindAsButton x evt `styleBasic` [bgColor blue] <> cardTextColor

cardAsButton :: Card -> WidgetNode s AppEvent
cardAsButton card@Card {id = idx, kind = ck} =
  case ck of
    CWild x -> wildCardAsButton x $ AppClickCard card
    CColored x -> coloredCardAsButton x $ AppClickCard card

gameBoard ::
  ( Traversable t1,
    Traversable t2,
    HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "computer" r3 r2,
    HasField "deck" r3 (t1 Card),
    HasField "drawPile" r3 (t2 Card),
    HasField "player" r3 r1,
    HasField "gameState" r4 r3
  ) =>
  p ->
  r4 ->
  WidgetNode s AppEvent
gameBoard wenv model =
  scroll $
    vstack
      [ hstack $ cardAsButton <$> V.toList model.gameState.computer.hand,
        spacer,
        vstack
          [ hstack $ cardAsButton <$> model.gameState.deck,
            spacer,
            hstack $ cardAsButton <$> model.gameState.drawPile
          ],
        spacer,
        hstack $ cardAsButton <$> V.toList model.gameState.player.hand
      ]
      `styleBasic` [padding 10]

playScene ::
  ( Traversable t2,
    Traversable t1,
    TS.TextShow a1,
    TS.TextShow a2,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "hand" r2 (Vector Card),
    HasField "hand" r1 (Vector Card),
    HasField "computer" r3 r2,
    HasField "deck" r3 (t1 Card),
    HasField "drawPile" r3 (t2 Card),
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

unsafeF :: Natural -> ([Card], Vector Card) -> ([Card], Vector Card)
unsafeF n x =
  fromMaybe ([], V.empty) $
    (execState (sequence $ drawNFromDeck n) . Just) x

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

      n = 1
      (xs, ph) = unsafeF n (model.gameState.deck, V.empty)
      (_, ch) = unsafeF n (xs, V.empty)
      dealer = pickDealer (ph ! 0) (ch ! 0)
  AppDealCards ->
    [ Model $
        model
          & #gameState . #player . #hand .~ ph
          & #gameState . #computer . #hand .~ ch
          & #gameState . #deck .~ xs''
          & #gameState . #drawPile .~ [tc ! 0]
    ]
    where
      n = 7
      (xs, ph) = unsafeF n (model.gameState.deck, V.empty)
      (xs', ch) = unsafeF n (xs, V.empty)
      (xs'', tc) = unsafeF n (xs', V.empty)
  AppClickCard card@Card {id = idx, kind = ck} ->
    case ck of
      CWild _ -> []
      CColored cc ->
        let hc = fromMaybe card $ V.find (\x -> x.id == idx) model.gameState.player.hand
         in case getCardColor hc of
              Nothing -> []
              Just x ->
                if eqColor x cc
                  then
                    let nh = V.filter (\c -> c.id /= idx) model.gameState.player.hand
                        -- This cannot fail (player selection), so we default to the same card
                        nc = [hc] <> model.gameState.drawPile
                     in [ Model $
                            model
                              & #gameState . #player . #hand .~ nh
                              & #gameState . #drawPile .~ nc
                        ]
                  else []
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