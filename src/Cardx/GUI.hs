{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardx.GUI (launchGUI) where

import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.Model
import Cardx.Util qualified as CU
import Cardx.WildKind (WildKind (..))
import Data.Default.Class qualified as D
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Optics ((%), (&), (.~), (?~), (^.))
import Relude hiding (id, (&))
import System.Random qualified as R
import System.Random.Shuffle qualified as RS
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
  = AppIgnore
  | AppPickDealer
  | AppDealCards
  | AppClickDeckCard
  | AppClickHandCard Card
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

specialCardStyle :: [StyleState]
specialCardStyle = cardTextColor : [bgColor black]

-- TODO: Show score on cards
wcAsBtn :: Typeable e => Maybe ColoredCard -> WildCard -> e -> WidgetNode s e
wcAsBtn mwcc (WildCard {kind, score}) evt =
  btn `styleBasic` style
  where
    style =
      case mwcc of
        Nothing -> specialCardStyle
        Just wcc ->
          case wcc of
            RedCard _ -> cardTextColor : [bgColor red]
            YellowCard _ -> cardTextColor : [bgColor yellow]
            GreenCard _ -> cardTextColor : [bgColor green]
            BlueCard _ -> cardTextColor : [bgColor blue]
    btn = button (TS.showt kind) evt

wcAsUnkBtn :: Typeable e => WildCard -> e -> WidgetNode s e
wcAsUnkBtn (WildCard {}) evt =
  button "?" evt `styleBasic` specialCardStyle

ckAsBtn :: Typeable e => ColoredKind -> e -> WidgetNode s e
ckAsBtn (CKActionCard (ActionCard {kind, score})) = button $ TS.showt kind
ckAsBtn (CKFaceCard (FaceCard {kind, score})) = button $ TS.showt kind

ckAsUnkBtn :: Typeable e => ColoredKind -> e -> WidgetNode s e
ckAsUnkBtn (CKActionCard (ActionCard {})) = button "?"
ckAsUnkBtn (CKFaceCard (FaceCard {})) = button "?"

ccAsBtn :: Typeable e => ColoredCard -> e -> WidgetNode s e
ccAsBtn (RedCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor red])
ccAsBtn (YellowCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor yellow])
ccAsBtn (GreenCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor green])
ccAsBtn (BlueCard x) evt =
  ckAsBtn x evt `styleBasic` (cardTextColor : [bgColor blue])

ccAsUnkBtn :: Typeable e => ColoredCard -> e -> WidgetNode s e
ccAsUnkBtn cc evt = btn evt `styleBasic` specialCardStyle
  where
    btn = ckAsUnkBtn $ getColoredKind cc

cardAsBtn :: Typeable p => Maybe ColoredCard -> Card -> p -> WidgetNode s p
cardAsBtn wcc Card {kind} =
  case kind of
    CWild x -> wcAsBtn wcc x
    CColored x -> ccAsBtn x

cardAsUnkBtn :: Typeable e => Card -> e -> WidgetNode s e
cardAsUnkBtn Card {kind} =
  case kind of
    CWild x -> wcAsUnkBtn x
    CColored x -> ccAsUnkBtn x

gameBoard ::
  ( HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "computer" r3 r1,
    HasField "deck" r3 [Card],
    HasField "drawPile" r3 [Card],
    HasField "player" r3 r2,
    HasField "turn" r3 Turn,
    HasField "wildcardColor" r3 (Maybe ColoredCard),
    HasField "gameState" p r3
  ) =>
  p ->
  WidgetNode s AppEvent
gameBoard model =
  scroll $
    vstack
      [ separatorLine,
        spacer,
        hstack $
          ( \x ->
              if gs.turn == TPlayer
                then cardAsUnkBtn x AppIgnore
                else cardAsBtn Nothing x $ AppClickHandCard x
          )
            <$> V.toList gs.computer.hand,
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
              if gs.turn == TComputer
                then cardAsUnkBtn x AppIgnore
                else cardAsBtn Nothing x $ AppClickHandCard x
          )
            <$> V.toList gs.player.hand,
        spacer,
        separatorLine
      ]
      `styleBasic` [padding 10]
  where
    gs = model.gameState

playScene ::
  ( TS.TextShow a1,
    TS.TextShow a2,
    HasField "score" r1 a1,
    HasField "score" r2 a2,
    HasField "hand" r1 (Vector Card),
    HasField "hand" r2 (Vector Card),
    HasField "computer" r3 r1,
    HasField "deck" r3 [Card],
    HasField "drawPile" r3 [Card],
    HasField "player" r3 r2,
    HasField "turn" r3 Turn,
    HasField "wildcardColor" r3 (Maybe ColoredCard),
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
    [ label $ "Pick " <> kt <> " color:",
      spacer,
      btn (AppPickWildCardColor $ RedCard ck) `styleBasic` [bgColor red],
      btn (AppPickWildCardColor $ YellowCard ck) `styleBasic` [bgColor yellow],
      btn (AppPickWildCardColor $ GreenCard ck) `styleBasic` [bgColor green],
      btn (AppPickWildCardColor $ BlueCard ck) `styleBasic` [bgColor blue]
    ]
  where
    kt = maybe "" TS.showt $ model.gameState.wildcardKind
    ck = CKFaceCard $ FaceCard {kind = 0, score = 1}
    btn = button ""

handFromTurn :: (IsLabel "computer" a, IsLabel "player" a) => Turn -> a
handFromTurn TPlayer = #player
handFromTurn TComputer = #computer

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

handleSpecialDrawCards :: AppModel -> Natural -> AppModel
handleSpecialDrawCards model n =
  model
    & #gameState % hft % #hand .~ h
    & #gameState % #deck .~ d
  where
    gs = model.gameState
    hft = handFromTurn $ nextTurn gs.turn
    (d, h) = unsafeF n (gs.deck, model ^. #gameState % hft % #hand)

shuffleCards :: R.RandomGen gen => [a] -> gen -> [a]
shuffleCards deck = RS.shuffle' deck (length deck)

resetEmptyDeck :: R.RandomGen gen => gen -> [a] -> [a] -> ([a], [a])
resetEmptyDeck rng drawPile [] =
  case drawPile of
    -- This shouldn't happen
    [] -> ([], [])
    (x : xs) ->
      -- TODO: Check if we need to increment seed everytime.
      ([x], shuffleCards xs rng)
resetEmptyDeck _ drawPile deck = (drawPile, deck)

updatedWildCardInfo model scc =
  case mwcc of
    Nothing -> Just model
    Just wcc ->
      if eqColor scc wcc
        then
          model
            & #gameState % #wildcardColor .~ Nothing
            & #gameState % #wildcardKind .~ Nothing
            & Just
        else Nothing
  where
    mwcc = model.gameState.wildcardColor

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt =
  let gs = model.gameState
   in case evt of
        AppIgnore -> []
        AppPickDealer ->
          [ Model $
              model
                & #hasPickedDealer .~ True
                & #gameState % #dealer .~ dealer
                & #gameState % #turn .~ firstTurn dealer,
            Event AppDealCards
          ]
          where
            n = 1
            (d, ph) = unsafeF n (shuffleCards gs.deck gs.rng, V.empty)
            (_, ch) = unsafeF n (d, V.empty)
            dealer = pickDealer (ph ! 0) (ch ! 0)
        AppDealCards ->
          -- TODO: If top card is wildcard, decide a random color for it or let
          -- whoever in next turn choose.
          [ Model $
              model
                & #gameState % #player % #hand .~ ph
                & #gameState % #computer % #hand .~ ch
                & #gameState % #deck .~ d''
                & #gameState % #drawPile .~ [tc ! 0]
          ]
          where
            n = 7
            (d, ph) = unsafeF n (shuffleCards gs.deck gs.rng, V.empty)
            (d', ch) = unsafeF n (d, V.empty)
            (d'', tc) = unsafeF 1 (d', V.empty)
        AppClickDeckCard ->
          [ Model $
              model
                & #gameState % handFromTurn gs.turn % #hand .~ h
                & #gameState % #drawPile .~ fst piles
                & #gameState % #deck .~ snd piles
          ]
          where
            (d, h) =
              unsafeF
                1
                ( gs.deck,
                  model ^. #gameState % handFromTurn gs.turn % #hand
                )
            piles = resetEmptyDeck gs.rng gs.drawPile d
        AppClickHandCard selectedCard@Card {id, kind = selectedCardKind} ->
          let pileTopCard = CU.defaultIfEmpty selectedCard gs.drawPile
           in ( if isValidPattern selectedCard pileTopCard
                  then
                    ( let nh = V.filter (\c -> c.id /= id) $ model ^. #gameState % handFromTurn gs.turn % #hand
                          -- This cannot fail (player selection), so we default to the same card
                          ndp = selectedCard : gs.drawPile
                          model' =
                            model
                              & #gameState % handFromTurn gs.turn % #hand .~ nh
                              & #gameState % #drawPile .~ ndp

                          toNextTurn m = m & #gameState % #turn .~ nextTurn gs.turn
                       in if V.null $ model' ^. #gameState % handFromTurn gs.turn % #hand
                            then [Event $ AppChangeScene SEnd]
                            else case selectedCardKind of
                              CWild (WildCard {kind}) ->
                                [ Model $
                                    model' & ((#gameState % #wildcardKind) ?~ kind),
                                  Event $ AppChangeScene SPickWildCardColor
                                ]
                              CColored scc ->
                                case getColoredKind scc of
                                  CKFaceCard _ ->
                                    maybe [] (\x -> [Model $ toNextTurn x]) $ updatedWildCardInfo model' scc
                                  CKActionCard (ActionCard {kind}) ->
                                    case kind of
                                      Skip ->
                                        maybe [] (\x -> [Model x]) $ updatedWildCardInfo model' scc
                                      Draw2 ->
                                        maybe
                                          []
                                          (\x -> [Model $ handleSpecialDrawCards x 2 & toNextTurn])
                                          $ updatedWildCardInfo model' scc
                    )
                  else []
              )
        AppPickWildCardColor cc ->
          case gs.wildcardKind of
            Nothing -> []
            Just wck ->
              case wck of
                Wild -> [Model $ model' & toNextTurn, Event $ AppChangeScene SPlay]
                WildDraw4 ->
                  [ Model $ handleSpecialDrawCards model' 4 & toNextTurn,
                    Event $ AppChangeScene SPlay
                  ]
          where
            model' = model & ((#gameState % #wildcardColor) ?~ cc)
            toNextTurn m = m & #gameState % #turn .~ nextTurn gs.turn
        AppChangeScene scene ->
          let changeScene s = Model $ model & #currentScene .~ s
           in case scene of
                SMenu -> [changeScene SMenu]
                SPickDealer -> [changeScene SPickDealer]
                SPlay -> [changeScene SPlay]
                SPickWildCardColor -> [changeScene SPickWildCardColor]
                SEnd -> [changeScene SEnd]

launchGUI :: IO ()
launchGUI = do
  rng <- R.newStdGen
  let model = initialModel & #gameState % #rng .~ rng
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppIgnore
      ]
