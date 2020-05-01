module PrsESpec where

import Test.Hspec
import PrsE

spec ::Spec
spec = 
  describe "Prog 1.4.3 - Parser with error" $
    do 
      it "(charE 'A') \"ABC\"" $
        runPrsE (charE 'A') "ABC" `shouldBe` Right ('A',"BC")
      it "(charE 'A') \"BCD\"" $
        runPrsE (charE 'A') "BCD" `shouldBe` Left "unexpected B"
      it "(charE 'A') \"\"" $
        runPrsE (charE 'A') ""    `shouldBe` Left "unexpected end of input"

