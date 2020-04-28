{-# LANGUAGE UnicodeSyntax #-}

module ArrnSpec where

import Arrn
import Test.Hspec

spec :: Spec
spec = do
  describe "Prog 1.1.1 - instance Functor Arr2 and Arr3 " $
    do
      it "Arr2 is Functor" $
        getArr2 (fmap length (Arr2 take)) 10 "abc"
          `shouldBe` 3
      it "Arr3 is Functor" $
        getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
          `shouldBe` [33,44]
    
  describe "Prog 1.2.3 - instance Applicative Arr2 and Arr3" $
    do
      it "Arr2 is Applicative" $
        getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
          `shouldBe` -1
      it "Arr3 is Applicative" $
        getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
          `shouldBe` -15