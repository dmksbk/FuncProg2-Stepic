module ZipListUtilsSpec where

import ZipListUtils
import Test.Hspec

spec :: Spec
spec = describe "Prog 1.2.1 - operators hiding ZipList packing/unpacking" $
  do
  let x1s = [1,2,3]
  let x2s = [4,5,6]
  let x3s = [7,8,9]
  let x4s = [10,11,12]
  
  it "Zip with 2 elements" $
    (\a b -> 2*a+3*b) >$< x1s >*< x2s
      `shouldBe` [14,19,24]
  it "Zip with 3 elements" $
    (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
      `shouldBe` [49,59,69]
  it "Zi[ with 4 elements" $
    (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
      `shouldBe` [9,15,21]


