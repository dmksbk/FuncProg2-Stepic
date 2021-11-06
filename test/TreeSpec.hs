module TreeSpec where

import Tree
    ( Levelorder(LevelO),
      Postorder(PostO),
      Preorder(PreO),
      Tree(Nil, Branch) )
import Data.Foldable ( sequenceA_ )
import Test.Hspec ( describe, it, shouldBe, Spec )

spec :: Spec
spec = do
  describe "Prog 2.1.5 - Foldable Tree" $
    do
      let tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
    --   Tree structure:
    --        ┌── 3 ──┐
    --      ⋄ 1 ┐   ⋄ 4 ⋄
    --        ⋄ 2 ⋄        
    
      it "In-order fold" $
        foldr (:) [] tree `shouldBe` [1,2,3,4]
        
      it "Pre-order fold" $
        foldr (:) [] (PreO tree) `shouldBe` [3,1,2,4]

      it "Post-order fold" $
        foldr (:) [] (PostO tree) `shouldBe` [2,1,4,3]

      it "Level-order fold" $
        foldr (:) [] (LevelO tree) `shouldBe` [3,1,4,2]
  
  describe "Question 2.2.3" $
    do
      let tree = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
      it "sequenceA_ on preordered tree" $
        fst (sequenceA_ $ (\x -> (show x,x)) <$> PreO tree) `shouldBe` "21435"
