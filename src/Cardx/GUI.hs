{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardx.GUI (launchGUI) where

import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.GUI.Types
import Cardx.GUI.View qualified as CGV
import Cardx.Model
import Cardx.Util qualified as CU
import Cardx.WildKind (WildKind (..))
import Data.Default.Class qualified as D
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import GHC.Records (HasField)
import Monomer
import Optics ((%), (&), (.~), (?~), (^.))
import Optics.Internal.Optic.Subtyping qualified
import Optics.Internal.Optic.Types qualified
import Optics.Label qualified
import Relude hiding (id, (&))
import System.Random qualified as R
import System.Random.Shuffle qualified as RS
import TextShow qualified as TS

playScene model =
  vstack
    [ label $ "Next turn: " <> (TS.showt . nextTurn) gs.turn,
      spacer,
      CGV.gameBoard model
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

handFromTurn :: (IsLabel "player2" a, IsLabel "player1" a) => Turn -> a
handFromTurn TPlayer1 = #player1
handFromTurn TPlayer2 = #player2

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI _ model = widgetTree
  where
    widgetTree =
      vstack
        [ case model.currentScene of
            SPickDealer -> CGV.pickDealerScene model
            SPlay -> playScene model
            SPickWildCardColor -> pickWildCardColorScene model
            SEndRound -> CGV.endScene model
        ]
        `styleBasic` [padding 10]

initialModel :: AppModel
initialModel = AppModel D.def SPickDealer False $ R.mkStdGen 0

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

sumRoundWinnerScore ::
  ( Optics.Label.LabelOptic "player1" l1 u1 v1 u2 v2,
    Optics.Label.LabelOptic "player2" l1 u1 v1 u2 v2,
    HasField "turn" r Turn,
    HasField "gameState" p r,
    Optics.Internal.Optic.Subtyping.Is
      k1
      Optics.Internal.Optic.Types.A_Getter,
    Optics.Internal.Optic.Subtyping.JoinKinds k2 l2 k1,
    Optics.Internal.Optic.Subtyping.JoinKinds k3 l1 k2,
    Optics.Label.LabelOptic
      "hand"
      l2
      u2
      v2
      (Vector Card)
      (Vector Card),
    Optics.Label.LabelOptic "gameState" k3 p p u1 v1
  ) =>
  p ->
  Natural
sumRoundWinnerScore model =
  s
  where
    other = handFromTurn $ nextTurn model.gameState.turn
    wh = model ^. #gameState % other % #hand
    s = V.foldl' (\a b -> cardScore b + a) 0 wh

toNextTurn ::
  ( Optics.Internal.Optic.Subtyping.JoinKinds k1 l k2,
    Optics.Internal.Optic.Subtyping.Is
      k2
      Optics.Internal.Optic.Types.A_Setter,
    Optics.Label.LabelOptic "turn" l u v a Turn,
    Optics.Label.LabelOptic "gameState" k1 r1 b u v,
    HasField "turn" r2 Turn,
    HasField "gameState" r1 r2
  ) =>
  r1 ->
  b
toNextTurn model = model & #gameState % #turn .~ nextTurn model.gameState.turn

handleColoredCards :: ColoredCard -> AppModel -> [EventResponse AppModel e sp ep]
handleColoredCards scc model =
  case getColoredKind scc of
    CKFaceCard _ ->
      maybe [] (\x -> [Model $ toNextTurn x]) $ updatedWildCardInfo model scc
    CKActionCard (ActionCard {kind}) ->
      case kind of
        Skip ->
          maybe [] (\x -> [Model x]) $ updatedWildCardInfo model scc
        Draw2 ->
          maybe
            []
            (\x -> [Model $ handleSpecialDrawCards x 2 & toNextTurn])
            $ updatedWildCardInfo model scc

handleRoundEnd model =
  [ Model $
      model
        & #gameState % #progression
          .~ ( if ps >= CC.maxScore
                 then GPWin
                 else GPRound RPWin
             )
        & #gameState % hft % #score .~ ps,
    Event $ AppChangeScene SEndRound
  ]
  where
    hft = handFromTurn $ model ^. #gameState % #turn
    ps = sumRoundWinnerScore model

onClickHandCard ::
  Card ->
  CardKind ->
  Natural ->
  AppModel ->
  [EventResponse AppModel AppEvent sp ep]
onClickHandCard selectedCard selectedCardKind selectedCardId model =
  let gs = model.gameState
      pileTopCard = CU.defaultIfEmpty selectedCard gs.drawPile
   in ( if isValidPattern selectedCard pileTopCard
          then
            ( let nh = V.filter (\c -> c.id /= selectedCardId) $ model ^. #gameState % handFromTurn gs.turn % #hand
                  -- This cannot fail (player selection), so we default to the same card
                  ndp = selectedCard : gs.drawPile
                  hft m = handFromTurn $ m ^. #gameState % #turn
                  model' =
                    model
                      & #gameState % hft model % #hand .~ nh
                      & #gameState % #drawPile .~ ndp
               in if V.null $ model' ^. #gameState % hft model' % #hand
                    then handleRoundEnd model'
                    else case selectedCardKind of
                      CWild (WildCard {kind}) ->
                        [ Model $
                            model' & ((#gameState % #wildcardKind) ?~ kind),
                          Event $ AppChangeScene SPickWildCardColor
                        ]
                      CColored scc -> handleColoredCards scc model'
            )
          else []
      )

onDealCards model =
  case f topCard of
    CWild (WildCard {kind}) ->
      [ Model $
          model' & ((#gameState % #wildcardKind) ?~ kind),
        Event $ AppChangeScene SPickWildCardColor
      ]
    CColored _ -> [Model model']
  where
    gs = model.gameState
    n = 7
    (d, ph) = unsafeF n (shuffleCards gs.deck gs.rng, V.empty)
    (d', ch) = unsafeF n (d, V.empty)
    (d'', tc) = unsafeF 1 (d', V.empty)
    topCard = tc ! 0
    f Card {kind} = kind
    model' =
      model
        & #gameState % #player1 % #hand .~ ph
        & #gameState % #player2 % #hand .~ ch
        & #gameState % #deck .~ d''
        & #gameState % #drawPile .~ [topCard]

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
        AppQuitGame -> [Request $ ExitApplication True]
        AppPickDealer ->
          [ Model $
              model
                & #hasPickedDealer .~ True
                & #gameState % #dealer .~ dealer
                & #gameState % #turn .~ firstTurn dealer
                & #gameState % #rng .~ old
                & #oldRng .~ new,
            Event AppDealCards
          ]
          where
            n = 1
            (old, new) = R.split model.oldRng
            (d, ph) = unsafeF n (shuffleCards gs.deck old, V.empty)
            (_, ch) = unsafeF n (d, V.empty)
            dealer = pickDealer (ph ! 0) (ch ! 0)
        AppDealCards -> onDealCards model
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
        AppClickHandCard card@Card {id, kind} -> onClickHandCard card kind id model
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
        AppResetRound ->
          [ Model $
              initialModel
                & #oldRng .~ oldRng
                & #gameState % #player1 % #score .~ p1s
                & #gameState % #player2 % #score .~ p2s,
            Event $ AppChangeScene SPickDealer
          ]
          where
            oldRng = model.oldRng
            p1s = model.gameState.player1.score
            p2s = model.gameState.player2.score
        AppChangeScene scene ->
          let changeScene s = Model $ model & #currentScene .~ s
           in case scene of
                SPickDealer -> [changeScene SPickDealer]
                SPlay -> [changeScene SPlay]
                SPickWildCardColor -> [changeScene SPickWildCardColor]
                SEndRound -> [changeScene SEndRound]

launchGUI :: IO ()
launchGUI = do
  rng <- R.newStdGen
  let model = initialModel & #gameState % #rng .~ rng & #oldRng .~ rng
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Cardx",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppIgnore
      ]
