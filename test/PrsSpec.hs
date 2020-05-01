module PrsSpec where

import Test.Hspec
import Prs
import Data.Char (digitToInt)

spec :: Spec
spec =
  describe "Prog 1.4.1 - Functor Prs" $
    do
      it "Prs is Functor (Just Char)" $
        runPrs anyChr "ABC" `shouldBe` Just ('A',"BC")
      it "Prs is Functor (Nothing)" $
         runPrs anyChr "" `shouldBe` Nothing
      it "Prs is Functor (Just Int)" $
        runPrs (digitToInt <$> anyChr) "BCD" `shouldBe` Just (11,"CD")

