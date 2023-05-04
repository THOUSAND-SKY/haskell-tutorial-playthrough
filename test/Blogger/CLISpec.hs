module Blogger.CLISpec where

import Blogger.CLI (outputWriter)
import Blogger.CLI.Parser (SingleOutput (..))
import System.IO (stdout)
import Test.Hspec

spec :: Spec
spec = do
  describe "cli" $ do
    it "parses intended output correctly" $ do
      outputWriter Stdout `shouldReturn` stdout

-- no Show instance for IO Handle, dunno if can compare.
-- let a = outputWriter (OutputFile "asd")
-- let b = openFile "asd" WriteMode
-- a `shouldBe` b
-- outputWriter (OutputFile "asd") `shouldBe` openFile "asd" WriteMode
