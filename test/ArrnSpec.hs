module ArrnSpec where

import Arrn
import Test.Hspec

spec :: Spec
spec = 
  describe "instance Functor Arr2 and Arr3 " $ do
    it "Arr2 is a functor" $
      getArr2 (fmap length (Arr2 take)) 10 "abc"
        `shouldBe` 3
    
    it "Arr3 is a functor" $
      getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
        `shouldBe` [33,44]
