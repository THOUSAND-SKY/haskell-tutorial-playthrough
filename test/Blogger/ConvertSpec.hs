module Blogger.ConvertSpec where

import Blogger.Convert
import Blogger.Env (blankEnv)
import Blogger.Html.Html (render)
import Blogger.Markup.Markup (parse)
import Test.Hspec

spec :: Spec
spec = do
  describe "convert" $ do
    it "produces content" $ do
      render (convert blankEnv "My page title" (parse (unlines ["* test", "hi"])))
        `shouldBe` "<html><head><title> - My page title</title><style src=\"\"></style></head><body><h1>test</h1><p>hi</p></body></html>"
