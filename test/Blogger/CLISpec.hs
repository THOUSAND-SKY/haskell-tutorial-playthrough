module Blogger.CLISpec where

import Blogger.CLI
import System.Environment ()
import Test.Hspec

spec :: Spec
spec = do
  describe "cli" $ do
    it "reads args" $ do
      getInput ["q", "b"]
        >>= \x -> x `shouldBe` ""
