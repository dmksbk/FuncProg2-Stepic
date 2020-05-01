module PrsESpec where

import Test.Hspec
import PrsE

spec ::Spec
spec = do
  describe "Prog 1.4.3 - Parser PrsE with error" $
    do 
      it "(charE 'A') \"ABC\"" $
        runPrsE (charE 'A') "ABC" `shouldBe` Right ('A',"BC")
      it "(charE 'A') \"BCD\"" $
        runPrsE (charE 'A') "BCD" `shouldBe` Left "unexpected B"
      it "(charE 'A') \"\"" $
        runPrsE (charE 'A') ""    `shouldBe` Left "unexpected end of input"

  describe "Prog 1.4.4 - Applicative PrsE" $
    do
      let anyE = satisfyE (const True)
      it "Right (('A','C'),\"DE\")" $
        runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
          `shouldBe` Right (('A','C'),"DE")
      it "Left \"unexpected B\"" $
        runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
          `shouldBe` Left "unexpected B"
      it "Left \"unexpected end of input\"" $
        runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
          `shouldBe` Left "unexpected end of input"
