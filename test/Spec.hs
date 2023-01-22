import Cardx.ActionCard (ActionCard (..))
import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.FaceCard (FaceCard (..))
import Cardx.Model (Card (..), ColoredCard (..), ColoredKind (..), Dealer (..), Turn (..))
import Cardx.Model qualified as CM
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind (..))
import Data.Vector qualified as V
import Relude
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Model.hs" $ do
    it "makeWilds" $ do
      CM.makeWilds Wild `shouldBe` V.fromList [CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50})]

    it "makeRange" $ do
      CM.makeRange 0 (\x -> RedCard (CKFaceCard (FaceCard x x))) (V.fromList [RedCard (CKActionCard (ActionCard {kind = Draw2, score = CC.actionScore})), RedCard (CKActionCard (ActionCard {kind = Skip, score = CC.actionScore}))])
        `shouldBe` V.fromList
          [ RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20})),
            RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})),
            RedCard (CKFaceCard (FaceCard {kind = 0, score = 0})),
            RedCard (CKFaceCard (FaceCard {kind = 1, score = 1})),
            RedCard (CKFaceCard (FaceCard {kind = 2, score = 2})),
            RedCard (CKFaceCard (FaceCard {kind = 3, score = 3})),
            RedCard (CKFaceCard (FaceCard {kind = 4, score = 4})),
            RedCard (CKFaceCard (FaceCard {kind = 5, score = 5})),
            RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})),
            RedCard (CKFaceCard (FaceCard {kind = 7, score = 7})),
            RedCard (CKFaceCard (FaceCard {kind = 8, score = 8})),
            RedCard (CKFaceCard (FaceCard {kind = 9, score = 9}))
          ]
    it "makeColoredCardSet" $ do
      CM.makeColoredCardSet 0 GreenCard (V.fromList [ActionCard {kind = Skip, score = CC.actionScore}, ActionCard {kind = Skip, score = CC.actionScore}, ActionCard {kind = Draw2, score = CC.actionScore}])
        `shouldBe` V.fromList
          [ GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})),
            GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})),
            GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})),
            GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0})),
            GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})),
            GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})),
            GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})),
            GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})),
            GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})),
            GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})),
            GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})),
            GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})),
            GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9}))
          ]

    it "makeColoreds" $ do
      CM.makeColoreds
        `shouldBe` V.fromList
          [ CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
            CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
            CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))
          ]
    it "makeDeck" $ do
      CM.makeDeck
        `shouldBe` [ CWild (WildCard {kind = Wild, score = 50}),
                     CWild (WildCard {kind = Wild, score = 50}),
                     CWild (WildCard {kind = Wild, score = 50}),
                     CWild (WildCard {kind = Wild, score = 50}),
                     CWild (WildCard {kind = WildDraw4, score = 50}),
                     CWild (WildCard {kind = WildDraw4, score = 50}),
                     CWild (WildCard {kind = WildDraw4, score = 50}),
                     CWild (WildCard {kind = WildDraw4, score = 50}),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 0, score = 0}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))),
                     CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8}))),
                     CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))
                   ]

  it "coloredScore" $ do
    CM.coloredScore (RedCard (CKFaceCard (FaceCard {kind = 1, score = 2939}))) `shouldBe` 2939

  it "cardScore (WildCard)" $ do
    CM.cardScore (CWild (WildCard {kind = WildDraw4, score = 20})) `shouldBe` 20

  it "cardScore (ColoredCard)" $ do
    CM.cardScore (CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 85})))) `shouldBe` 85

  it "pickDealer" $ do
    let pc = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 85})))
        cc = CWild (WildCard {kind = WildDraw4, score = 102})
    CM.pickDealer pc cc `shouldBe` DComputer

  it "nextTurn" $ do
    CM.nextTurn TComputer `shouldBe` TPlayer

  it "firstTurn" $ do
    CM.firstTurn DComputer `shouldBe` TPlayer

  it "drawCardFromDeck" $
    do
      let from = [CWild (WildCard Wild 0), CWild (WildCard Wild 0), CWild (WildCard WildDraw4 0)]
          to = V.empty
      CM.drawCardFromDeck from to
      `shouldBe` Just
        ( [CWild (WildCard Wild 0), CWild (WildCard WildDraw4 0)],
          V.fromList [CWild (WildCard Wild 0)]
        )
