{-# LANGUAGE DataKinds #-}

module Cardx.GUI.View.Internal (cardAsBtn, cardAsUnkBtn) where

import Cardx.Model
  ( ActionCard (ActionCard, kind, score),
    Card (Card, kind),
    CardKind (CColored, CWild),
    ColoredCard (..),
    ColoredKind (..),
    FaceCard (FaceCard, kind, score),
    WildCard (..),
    getColoredKind,
  )
import Monomer
  ( CmbBgColor (bgColor),
    CmbStyleBasic (styleBasic),
    CmbTextColor (textColor),
    StyleState,
    WidgetNode,
    black,
    blue,
    button,
    green,
    red,
    white,
    yellow,
  )
import Relude (Maybe (..), Typeable, ($))
import TextShow qualified as TS

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
  ckAsBtn x evt `styleBasic` (textColor black : [bgColor yellow])
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