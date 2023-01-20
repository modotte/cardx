import Cardx.ActionCard (ActionCard (..))
import Cardx.ActionKind (ActionKind (..))
import Cardx.Constant qualified as CC
import Cardx.FaceCard (FaceCard (..))
import Cardx.Model (Card (CWild), ColoredCard (..), ColoredKind (..))
import Cardx.Model qualified as CM
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind (Wild))
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
