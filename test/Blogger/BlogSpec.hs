module Blogger.BlogSpec where

import Blogger.Blog
import Blogger.Html.Html
import Blogger.Markup.Markup (Structure (Heading))
import Test.Hspec

type Error = String

spec :: Spec
spec = do
  describe "builds index" $ do
    it "makes link list" $ do
      let got = links [("test", [Heading 1 "a"])]
      let expected = ul_ [a_ "test" $ txt_ "a"]
      got `shouldBe` expected

-- it "implements >>= for IO (Either Error a)" $ do
--   let bindExceptT :: IO (Either Error a) -> (a -> IO (Either Error b)) -> IO (Either Error b)
--       bindExceptT ioa f = do
--         a <- ioa
--         case a of
--           Left e -> return $ Left e
--           Right d -> f d
--   True `shouldBe` True
--         -- where
--         --   go ioAction = do
--         --     a <- ioAction
--             -- case a of
--             --   Left e -> return $ Left e
--             --   Right d -> do
