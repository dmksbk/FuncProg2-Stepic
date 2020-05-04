module PrsSpec where

import Test.Hspec
import Prs
import Data.Char (digitToInt)
import Control.Applicative((<|>))

spec :: Spec
spec = do
  describe "Prog 1.4.1 - Functor Prs" $
    do
      it "Prs is Functor (Just Char)" $
        runPrs anyChr "ABC" `shouldBe` Just ('A',"BC")
      it "Prs is Functor (Nothing)" $
         runPrs anyChr "" `shouldBe` Nothing
      it "Prs is Functor (Just Int)" $
        runPrs (digitToInt <$> anyChr) "BCD" `shouldBe` Just (11,"CD")

  describe "Prog 1.4.2 - Applicative Prs" $
    do
      it "Parse 3 chars" $
        runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
          `shouldBe` Just (('A','B','C'),"DE")
      it "Parse 2 chars, ignoring first one" $
        runPrs (anyChr *> anyChr) "ABCDE"
          `shouldBe` Just ('B',"CDE")
  
  describe "Prog 1.4.5 - Alternative Prs" $
    do
      it "A|B on ABC" $
        runPrs (char 'A' <|> char 'B') "ABC"
          `shouldBe` Just ('A',"BC")
      it "A|B on BCD" $
        runPrs (char 'A' <|> char 'B') "BCD"
          `shouldBe` Just ('B',"CD")
      it "A|B on CDE" $
        runPrs (char 'A' <|> char 'B') "CDE"
          `shouldBe` Nothing
  describe "Prog 1.4.6 - many and many1 for Prs" $
    do
      it "many1 on \"AAABCDE\"" $
        runPrs (many1 $ char 'A') "AAABCDE"
           `shouldBe` Just ("AAA","BCDE")
      it "many1 on \"BCDE\"" $
        runPrs (many1 $ char 'A') "BCDE"
          `shouldBe` Nothing
  
  describe "Prog 1.4.7 - mul and nat parser for Prs" $
    do
      it "14 * 3 = 42" $
        runPrs mult "14*3"
           `shouldBe` Just (42,"")
      it "64 * 32 = 2048" $
        runPrs mult "64*32"
          `shouldBe` Just (2048,"")
      it "77 * 0 = 0" $
        runPrs mult "77*0"
          `shouldBe` Just (0,"")
      it "2 * 77 = 154" $
        runPrs mult "2*77AAA"
          `shouldBe` Just (154,"AAA")