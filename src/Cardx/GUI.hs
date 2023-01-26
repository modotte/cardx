{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Relude hiding (id, (&))
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
  | SPickWildCardColor
  | SEnd
  deriving (Show, Eq)

data AppEvent
  = AppInit
  | AppPickDealer
  | AppDealCards
  | AppClickCard Card
  | AppPickWildCardColor ColoredCard
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

cardTextColor :: StyleState
cardTextColor = textColor white

-- TODO: Show score on cards
wcAsBtn :: Typeable e => WildCard -> e -> WidgetNode s e
wcAsBtn (WildCard {kind, score}) evt =
  button (TS.showt kind) evt `styleBasic` (cardTextColor : [bgColor black])

ckAsBtn :: Typeable e => ColoredKind -> e -> WidgetNode s e
ckAsBtn (CKActionCard (ActionCard {kind, score})) = button $ TS.showt kind
ckAsBtn (CKFaceCard (FaceCard {kind, score})) = button $ TS.showt kind

ccAsBtn :: Typeable e => ColoredCard -> e -> WidgetNode s e
ccAsBtn (RedCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor red])
ccAsBtn (YellowCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor yellow])
ccAsBtn (GreenCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor green])
ccAsBtn (BlueCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor blue])

cardAsBtn :: Card -> WidgetNode s AppEvent
cardAsBtn card@Card {kind} =
  case kind of
    CWild x -> wcAsBtn x $ AppClickCard card
    CColored x -> ccAsBtn x $ AppClickCard card

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
  r4 ->
  WidgetNode s AppEvent
gameBoard model =
  scroll $
    vstack
      [ hstack $ cardAsBtn <$> V.toList gs.computer.hand,
        spacer,
        vstack
          [ hstack $ cardAsBtn <$> gs.deck,
            spacer,
            hstack $ cardAsBtn <$> gs.drawPile
          ],
        spacer,
        hstack $ cardAsBtn <$> V.toList gs.player.hand
      ]
      `styleBasic` [padding 10]
  where
    gs = model.gameState

playScene ::
  ( Traversable t2,
    Traversable t1,
    TS.TextShow a1,
    TS.TextShow a2,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "computer" r3 r1,
    HasField "deck" r3 (t1 Card),
    HasField "drawPile" r3 (t2 Card),
    HasField "player" r3 r2,
    HasField "turn" r3 Turn,
    HasField "gameState" r4 r3
  ) =>
  r4 ->
  WidgetNode s AppEvent
playScene model =
  vstack
    [ label $ "Win score: " <> TS.showt CC.maxScore,
      label $ "Player score: " <> TS.showt gs.player.score,
      label $ "Computer score: " <> TS.showt gs.computer.score,
      label $ "Next turn: " <> (TS.showt . nextTurn) gs.turn,
      spacer,
      gameBoard model
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
    [ label $ "Pick " <> kindText <> " color.",
      spacer,
      button "" (AppPickWildCardColor $ RedCard defCK) `styleBasic` [bgColor red],
      button "" (AppPickWildCardColor $ YellowCard defCK) `styleBasic` [bgColor yellow],
      button "" (AppPickWildCardColor $ GreenCard defCK) `styleBasic` [bgColor green],
      button "" (AppPickWildCardColor $ BlueCard defCK) `styleBasic` [bgColor blue]
    ]
  where
    kindText = maybe "" TS.showt $ model.gameState.wildcardKind
    defCK = CKFaceCard $ FaceCard {kind = 0, score = 1}

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI _ model = widgetTree
  where
    widgetTree =
      vstack
        [ case model.currentScene of
            SMenu -> menuScene
            SPickDealer -> pickDealerScene model
            SPlay -> playScene model
            SPickWildCardColor -> pickWildCardColorScene model
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
handleEvent _ _ model evt = case evt of
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
      (xs'', tc) = unsafeF 1 (xs', V.empty)
  AppClickCard selectedCard@Card {id, kind = selectedCardKind} ->
    let gs = model.gameState
        pileTopCard =
          case gs.drawPile of
            [] -> selectedCard
            (x : _) -> x
     in ( if isMatchShape selectedCard pileTopCard
            then
              ( let nh = V.filter (\c -> c.id /= id) gs.player.hand
                    -- This cannot fail (player selection), so we default to the same card
                    ndp = selectedCard : gs.drawPile
                    model' =
                      model
                        & #gameState . #player . #hand .~ nh
                        & #gameState . #drawPile .~ ndp
                 in case selectedCardKind of
                      CWild (WildCard {kind}) ->
                        [ Model $
                            model' & ((#gameState . #wildcardKind) ?~ kind),
                          Event $ AppChangeScene SPickWildCardColor
                        ]
                      CColored scc ->
                        case gs.wildcardColor of
                          Nothing -> [Model model']
                          Just wcc ->
                            -- TODO: Decide if want to allow non-face card stacking.
                            if eqColor scc wcc
                              then
                                [ Model $
                                    model'
                                      & #gameState . #wildcardColor .~ Nothing
                                      & #gameState . #wildcardKind .~ Nothing
                                ]
                              else []
              )
            else []
        )
  AppPickWildCardColor cc ->
    [ Model $ model & ((#gameState . #wildcardColor) ?~ cc),
      Event $ AppChangeScene SPlay
    ]
  AppChangeScene scene ->
    let changeScene s = Model $ model & #currentScene .~ s
     in case scene of
          SMenu -> [Model initialModel]
          SPickDealer -> [changeScene SPickDealer]
          SPlay -> [changeScene SPlay]
          SPickWildCardColor -> [changeScene SPickWildCardColor]
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