import Cardx.Model (Card (CWild), makeWilds)
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind (Wild, WildDraw4))
import Data.Vector qualified as V
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "" $ do
    it "" $ do
      makeWilds Wild `shouldBe` V.fromList [CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50}), CWild (WildCard {kind = Wild, score = 50})]
