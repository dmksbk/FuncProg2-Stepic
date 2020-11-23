module TripleSpec where

import Triple
import Test.Hspec ( describe, it, shouldBe, Spec )

spec :: Spec
spec = do
  describe "Prog 1.1.2 - instance Functor Triple and instance Applicative Functor" $
    do 
      it "Triple is Functor" $
        (^2) <$> Tr 1 (-2) 3 `shouldBe` Tr 1 4 9
        
      it "Triple is Applicative" $
        Tr (^2) (+2) (*3) <*> Tr 2 3 4 `shouldBe` Tr 4 5 12
    
  describe "Prog 2.1.3 - Сделайте тип Triple представителем класса типов Foldable" $
    do
      it "abcdefg!!" $
        foldr (++) "!!" (Tr "ab" "cd" "efg") `shouldBe` "abcdefg!!"
      it "!!abcdefg" $
        foldl (++) "!!" (Tr "ab" "cd" "efg") `shouldBe` "!!abcdefg"

