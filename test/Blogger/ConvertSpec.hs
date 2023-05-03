module Blogger.ConvertSpec where

import Blogger.Convert
import Blogger.Html.Internal (html_, render)
import Blogger.Markup.Markup (parse)
import Test.Hspec

wrap s = "<html><head><title></title></head><body>" <> s <> "</body></html>"

-- wrapTest = render . html_ ""

spec :: Spec
spec = do
  describe "convert" $ do
    it "produces content" $ do
      render (convert "My page title" (parse (unlines ["* test", "hi"])))
        `shouldBe` ("<html><head><title>My page title</title></head><body><h1>test</h1><p>hi</p></body></html>")
