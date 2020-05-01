module PrsSpec where

import Test.Hspec
import Prs
import Data.Char (digitToInt)

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