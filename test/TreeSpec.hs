module TreeSpec where

import Tree
import Test.Hspec ( describe, it, shouldBe, Spec )

spec :: Spec
spec = do
  describe "Prog 2.1.5 - Foldable Tree" $
    do
      let tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
      it "In-order fold" $
        foldr (:) [] tree `shouldBe` [1,2,3,4]
        
      it "Pre-order fold" $
        foldr (:) [] (PreO tree) `shouldBe` [3,1,2,4]

      it "Post-order fold" $
        foldr (:) [] (PostO tree) `shouldBe` [2,1,4,3]

      it "Level-order fold" $
        foldr (:) [] (LevelO tree) `shouldBe` [3,1,4,2]