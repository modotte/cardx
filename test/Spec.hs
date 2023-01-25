import Cardx.ActionCard (ActionCard (..))
import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.FaceCard (FaceCard (..))
import Cardx.Model (Card (..), CardKind (..), ColoredCard (..), ColoredKind (..), Dealer (..), Turn (..))
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
      CM.makeWilds Wild
        `shouldBe` V.fromList
          [ Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 50})},
            Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 50})},
            Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 50})},
            Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 50})}
          ]

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
          [ Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9})))}
          ]

    it "makeColoreds" $ do
      CM.makeColoreds
        `shouldBe` V.fromList
          [ Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
            Card {id = 0, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))}
          ]
    it "makeDeck" $ do
      CM.makeDeck
        `shouldBe` [ Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 50})},
                     Card {id = 1, kind = CWild (WildCard {kind = Wild, score = 50})},
                     Card {id = 2, kind = CWild (WildCard {kind = Wild, score = 50})},
                     Card {id = 3, kind = CWild (WildCard {kind = Wild, score = 50})},
                     Card {id = 4, kind = CWild (WildCard {kind = WildDraw4, score = 50})},
                     Card {id = 5, kind = CWild (WildCard {kind = WildDraw4, score = 50})},
                     Card {id = 6, kind = CWild (WildCard {kind = WildDraw4, score = 50})},
                     Card {id = 7, kind = CWild (WildCard {kind = WildDraw4, score = 50})},
                     Card {id = 8, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 9, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 10, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 11, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
                     Card {id = 12, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 13, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 14, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 15, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 16, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 17, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 18, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 19, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 20, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 21, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 22, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 23, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 24, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
                     Card {id = 25, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 26, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 27, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 28, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 29, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 30, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 31, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 32, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 33, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 34, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 35, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 36, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 37, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
                     Card {id = 38, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 39, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 40, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 41, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 42, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 43, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 44, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 45, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 46, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 47, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 48, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 49, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 50, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 0, score = 0})))},
                     Card {id = 51, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 52, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 53, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 54, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 55, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 56, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 57, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 58, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 59, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 60, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 61, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 62, kind = CColored (RedCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 63, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 64, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 65, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 66, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 67, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 68, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 69, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 70, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 71, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 72, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 73, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 74, kind = CColored (YellowCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 75, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 76, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 77, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 78, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 79, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 80, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 81, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 82, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 83, kind = CColored (YellowCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 84, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 85, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 86, kind = CColored (GreenCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 87, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 88, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 89, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 90, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 91, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 92, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 93, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 94, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 95, kind = CColored (GreenCard (CKFaceCard (FaceCard {kind = 9, score = 9})))},
                     Card {id = 96, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 97, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20})))},
                     Card {id = 98, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Draw2, score = 20})))},
                     Card {id = 99, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 1, score = 1})))},
                     Card {id = 100, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 2, score = 2})))},
                     Card {id = 101, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 3, score = 3})))},
                     Card {id = 102, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 4, score = 4})))},
                     Card {id = 103, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 5, score = 5})))},
                     Card {id = 104, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))},
                     Card {id = 105, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 7, score = 7})))},
                     Card {id = 106, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 8, score = 8})))},
                     Card {id = 107, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 9, score = 9})))}
                   ]

  it "coloredScore" $ do
    CM.coloredScore (RedCard (CKFaceCard (FaceCard {kind = 1, score = 2939}))) `shouldBe` 2939

  it "cardScore (WildCard)" $ do
    CM.cardScore (Card {id = 0, kind = CWild (WildCard {kind = WildDraw4, score = 20})}) `shouldBe` 20

  it "cardScore (ColoredCard)" $ do
    CM.cardScore (Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 85})))}) `shouldBe` 85

  it "pickDealer" $ do
    let pc = Card {id = 0, kind = CColored (BlueCard (CKActionCard (ActionCard {kind = Skip, score = 85})))}
        cc = Card {id = 0, kind = CWild (WildCard {kind = WildDraw4, score = 102})}
    CM.pickDealer pc cc `shouldBe` DComputer

  it "nextTurn" $ do
    CM.nextTurn TComputer `shouldBe` TPlayer

  it "firstTurn" $ do
    CM.firstTurn DComputer `shouldBe` TPlayer

  it "drawNFromDeck 1" $ do
    let from = [Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 1, kind = CWild (WildCard WildDraw4 0)}]
        to = V.empty
        f = execState (sequence $ CM.drawNFromDeck 1) $ Just (from, to)

    f
      `shouldBe` Just
        ( [Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 0})}, Card {id = 1, kind = CWild (WildCard {kind = WildDraw4, score = 0})}],
          V.fromList [Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 0})}]
        )
  it "drawNFromDeck 3" $ do
    let from = [Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 1, kind = CWild (WildCard WildDraw4 0)}]
        to = V.empty
        f = execState (sequence $ CM.drawNFromDeck 3) $ Just (from, to)

    f
      `shouldBe` Just
        ( [],
          V.fromList [Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 0})}, Card {id = 0, kind = CWild (WildCard {kind = Wild, score = 0})}, Card {id = 1, kind = CWild (WildCard {kind = WildDraw4, score = 0})}]
        )

  it "drawNFromDeck 10" $ do
    let from = [Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 0, kind = CWild (WildCard Wild 0)}, Card {id = 1, kind = CWild (WildCard WildDraw4 0)}]
        to = V.empty
        f = execState (sequence $ CM.drawNFromDeck 10) $ Just (from, to)

    f `shouldBe` Nothing

  it "eqColor (same kind)" $ do
    let x = BlueCard (CKActionCard (ActionCard {kind = Skip, score = 85}))
        y = BlueCard (CKActionCard (ActionCard {kind = Skip, score = 20}))

    CM.eqColor x y `shouldBe` True

  it "eqColor (different kind)" $ do
    let x = RedCard (CKActionCard (ActionCard {kind = Skip, score = 85}))
        y = RedCard (CKFaceCard (FaceCard {kind = 8, score = 20}))

    CM.eqColor x y `shouldBe` True

  it "isMatchShape (unmatching color)" $ do
    let x = Card {id = 56, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))}
        y = Card {id = 56, kind = CColored (RedCard (CKFaceCard (FaceCard {kind = 6, score = 6})))}

    CM.isMatchShape x y `shouldBe` False

  it "isMatchShape (colored and wildcard)" $ do
    let x = Card {id = 56, kind = CColored (BlueCard (CKFaceCard (FaceCard {kind = 6, score = 6})))}
        y = Card {id = 6, kind = CWild (WildCard {kind = WildDraw4, score = 50})}

    CM.isMatchShape x y `shouldBe` True