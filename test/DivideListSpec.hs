module DivideListSpec where

import Test.Hspec
import DivideList

spec :: Spec
spec = describe "Prog 1.2.2 - DivedeList" $
  do
  it "divideList" $ divideList [3,4,5]
    `shouldBe` 3.75
  it "divideList'" $
    divideList' [3,4,5]
      `shouldBe` ("<-3.0/<-4.0/<-5.0/1.0",3.75)


