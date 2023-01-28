{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardx.GUI (launchGUI) where

import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.Model
import Cardx.WildKind
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
  | AppIgnore
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

specialCardStyle :: [StyleState]
specialCardStyle = cardTextColor : [bgColor black]

-- TODO: Show score on cards
wcAsBtn :: Typeable e => WildCard -> e -> WidgetNode s e
wcAsBtn (WildCard {kind, score}) evt =
  button (TS.showt kind) evt `styleBasic` specialCardStyle

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

cardAsBtn :: Typeable e => Card -> e -> WidgetNode s e
cardAsBtn Card {kind} =
  case kind of
    CWild x -> wcAsBtn x
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
    HasField "gameState" p r3
  ) =>
  p ->
  WidgetNode s AppEvent
gameBoard model =
  scroll $
    vstack
      [ separatorLine,
        spacer,
        hstack $ (\x -> if gs.turn == TPlayer then cardAsUnkBtn x AppIgnore else cardAsBtn x $ AppClickCard x) <$> V.toList gs.computer.hand,
        spacer,
        separatorLine,
        spacer,
        hstack
          [ case gs.deck of
              [] -> label "Empty deck!"
              (x : _) -> hstack [cardAsBtn x $ AppClickCard x],
            spacer,
            separatorLine,
            spacer,
            case gs.drawPile of
              [] -> label "Empty draw pile!"
              (x : _) -> hstack [cardAsBtn x $ AppClickCard x]
          ],
        spacer,
        separatorLine,
        spacer,
        hstack $ (\x -> cardAsBtn x $ AppClickCard x) <$> V.toList gs.player.hand,
        spacer,
        separatorLine
      ]
      `styleBasic` [padding 10]
  where
    gs = model.gameState

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
    -- TODO: Check why we can't simply use `hft` to link the lens in setter.
    & #gameState . handFromTurn (nextTurn gs.turn) . #hand .~ h
    & #gameState . #deck .~ d
  where
    gs = model.gameState
    hft = handFromTurn $ nextTurn gs.turn
    (d, h) = unsafeF n (gs.deck, model ^. #gameState . hft . #hand)

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt =
  let gs = model.gameState
   in case evt of
        AppInit -> []
        AppIgnore -> []
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
            (d, ph) = unsafeF n (gs.deck, V.empty)
            (_, ch) = unsafeF n (d, V.empty)
            dealer = pickDealer (ph ! 0) (ch ! 0)
        AppDealCards ->
          [ Model $
              model
                & #gameState . #player . #hand .~ ph
                & #gameState . #computer . #hand .~ ch
                & #gameState . #deck .~ d''
                & #gameState . #drawPile .~ [tc ! 0]
          ]
          where
            n = 7
            (d, ph) = unsafeF n (gs.deck, V.empty)
            (d', ch) = unsafeF n (d, V.empty)
            (d'', tc) = unsafeF 1 (d', V.empty)
        AppClickCard selectedCard@Card {id, kind = selectedCardKind} ->
          let pileTopCard =
                case gs.drawPile of
                  [] -> selectedCard
                  (x : _) -> x
           in ( if isValidPattern selectedCard pileTopCard
                  then
                    ( let nh = V.filter (\c -> c.id /= id) $ model ^. #gameState . (handFromTurn gs.turn) . #hand
                          -- This cannot fail (player selection), so we default to the same card
                          ndp = selectedCard : gs.drawPile
                          model' =
                            model
                              & #gameState . (handFromTurn gs.turn) . #hand .~ nh
                              & #gameState . #drawPile .~ ndp

                          toNextTurn m = m & #gameState . #turn .~ nextTurn gs.turn
                          updatedWildcardInfo scc =
                            case gs.wildcardColor of
                              Nothing -> Just model'
                              Just wcc ->
                                -- TODO: Decide if want to allow non-face card stacking.
                                -- Also, I think we should discard WildDraw4 penalties for this game.
                                if eqColor scc wcc
                                  then
                                    model'
                                      & #gameState . #wildcardColor .~ Nothing
                                      & #gameState . #wildcardKind .~ Nothing
                                      & Just
                                  else Nothing
                       in case selectedCardKind of
                            CWild (WildCard {kind}) ->
                              [ Model $
                                  model' & ((#gameState . #wildcardKind) ?~ kind),
                                Event $ AppChangeScene SPickWildCardColor
                              ]
                            CColored scc ->
                              case getColoredKind scc of
                                CKFaceCard _ ->
                                  maybe [] (\x -> [Model $ toNextTurn x]) $ updatedWildcardInfo scc
                                CKActionCard (ActionCard {kind}) ->
                                  case kind of
                                    Skip ->
                                      maybe [] (\x -> [Model x]) $ updatedWildcardInfo scc
                                    Draw2 ->
                                      maybe
                                        []
                                        (\x -> [Model $ handleSpecialDrawCards x 2 & toNextTurn])
                                        $ updatedWildcardInfo scc
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
                  -- TODO: Check why we can't simply use `hft` to link the lens in setter
                  [ Model $ handleSpecialDrawCards model' 4 & toNextTurn,
                    Event $ AppChangeScene SPlay
                  ]
          where
            model' = model & ((#gameState . #wildcardColor) ?~ cc)
            toNextTurn m = m & #gameState . #turn .~ nextTurn gs.turn
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