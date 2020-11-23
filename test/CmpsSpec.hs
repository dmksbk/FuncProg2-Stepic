{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module CmpsSpec where

import Test.Hspec
import Cmps

spec :: Spec
spec = 
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
