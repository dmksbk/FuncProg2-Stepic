{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module CmpsSpec where

import Test.Hspec
import Cmps

spec :: Spec
spec = do
  describe "Prog 1.5.3 - unCmps3 and unCmps4" $ do
    let v1 :: ([] |.| [] |.| []) Int
        v1 = pure 42
    let v2 :: ([] |.| [] |.| []) Int
        v2 = Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
    it "Cmps [[[42]]]" $
      v1 `shouldBe` v2
    it "[[[42]]]" $
      unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
        `shouldBe` [[[42 :: Int]]]
    it "[Just [42]]" $
      unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
        `shouldBe` [Just [42 :: Int]]
    it "[[[[42]]]]" $
      unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
        `shouldBe` [[[[42 :: Int]]]]
    
  describe "My cases from course videos on Applicative Cmps" $ do
    it "Applicative Cmps case" $
      Cmps [Just (+1), Just (+2)] <*> Cmps [Just 30, Just 40]
        `shouldBe` Cmps [Just 31, Just 41, Just 32, Just 42]

  describe "Prog 2.1.15 - Foldable Cmps" $ do
    it "maximum $ Cmps [Nothing, Just 2, Just 3] => 3" $
      maximum (Cmps [Nothing, Just 2, Just 3])  `shouldBe` 3
    it "length $ Cmps [[1,2], [], [3,4,5,6,7]]   => 7" $
      length (Cmps [[1,2], [], [3,4,5,6,7]])    `shouldBe` 7