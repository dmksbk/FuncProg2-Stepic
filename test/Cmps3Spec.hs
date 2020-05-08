module Cmps3Spec where

import           Test.Hspec
import           Cmps3

spec :: Spec
spec = describe "Prog 1.5.2 - Cmps3 is a functor" $ do
  let c1 = Cmps3 [[[1], [2, 3, 4], [5, 6]], [], [[7, 8], [9, 10, 11]]]
  let c2 = Cmps3 [[[1], [4, 9, 16], [25, 36]], [], [[49, 64], [81, 100, 121]]]
  it "Cmps3 is a functor" $ fmap (^ 2) c1 `shouldBe` c2
