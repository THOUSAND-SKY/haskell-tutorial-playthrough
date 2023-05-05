module Blogger.DirectorySpec where

import Blogger.Directory
import Test.Hspec

spec :: Spec
spec = do
  describe "applyIoOnList" $ do
    it "should work on error" $ do
      x <-
        applyIoOnList
          -- Don't know how to type this one without the do-block.
          ( \_ -> do
              _ <- error "asd"
              return "a"
          )
          ["a"]
      x `shouldBe` [("a", Left "asd\nCallStack (from HasCallStack):\n  error, called at test/Blogger/DirectorySpec.hs:14:20 in main:Blogger.DirectorySpec")]

    it "should work when no error" $ do
      x <-
        applyIoOnList
          ( \_ -> do
              return "a"
          )
          ["a"]
      x `shouldBe` [("a", Right "a")]

  describe "filterAndReportFailures" $ do
    it "should filter and report failures" $ do
      filterAndReportFailures [("a", Left "b" :: Either String String)] `shouldReturn` []

    it "should return good ones" $ do
      filterAndReportFailures [("a", Right "b" :: Either String String)] `shouldReturn` [("a", "b")]
