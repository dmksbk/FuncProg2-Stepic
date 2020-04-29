module ApplicativeUtilsSpec where

import ApplicativeUtils
import Test.Hspec

spec :: Spec
spec =
  describe "Prog 1.2.4 - counterexamples for <**> and <*?>" $ do
    it "exprMaybe   HAS NO counterexample" $
      exprMaybe (<**>) `shouldBe` exprMaybe (<*?>)
    it "exprList    HAS    counterexample" $
      exprList (<**>) `shouldNotBe` exprList (<*?>)
    it "exprZipList HAS NO counterexample" $
      exprZipList (<**>) `shouldBe` exprZipList (<*?>)
    it "exprEither  HAS    counterexample" $
      exprEither (<**>) `shouldNotBe` exprEither (<*?>)
    it "exprPair    HAS    counterexample" $
      exprPair (<**>) `shouldNotBe` exprPair (<*?>)
    it "exprEnv     HAS NO counterexample" $
      exprEnv (<**>) "Hey" `shouldBe` exprEnv (<*?>) "Hey"