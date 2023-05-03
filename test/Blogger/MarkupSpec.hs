module Blogger.MarkupSpec where

import Blogger.Markup.Color
import Blogger.Markup.Markup
import Test.Hspec

typeTest :: Structure
typeTest = Paragraph "yep"

spec :: Spec
spec = do
  describe "color" $ do
    it "should find bright colors" $ do
      isBright (AnsiColor Dark Black) `shouldBe` False
      isBright (AnsiColor Bright Black) `shouldBe` True

    it "should parse" $ do
      parse (unlines ["a", "* b", "# c"]) `shouldMatchList` ([Paragraph "a", Heading 1 "b", OrderedList ["c"]] :: [Structure])
