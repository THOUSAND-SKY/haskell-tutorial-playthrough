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

    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]
