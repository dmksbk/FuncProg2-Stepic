{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}

module CmpsSpec where

import Test.Hspec
import Cmps

-- spec :: Spec
-- spec = 
--   describe "Prog 1.5.3 - unCmps3 and unCmps4" $ do
--     -- it "Cmps [[[42]]]" $
--     --   pure 42 :: ([] |.| [] |.| []) Int
--     --     `shouldBe` Cmps [[[42]]]
--     it "[[[42]]]" $
--       unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
--         `shouldBe` [[[42 :: Int]]]
--     it "[Just [42]]" $
--       unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
--         `shouldBe` [Just [42 :: Int]]
--     it "[[[[42]]]]" $
--       unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
--         `shouldBe` [[[[42 :: Int]]]]
