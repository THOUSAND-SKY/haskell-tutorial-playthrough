module Blogger.HtmlSpec where

import Blogger.Html.Html (html_, p_, render, ul_)
import Test.Hspec

wrap s = "<html><head><title></title></head><body>" <> s <> "</body></html>"

wrapTest = render . html_ ""

spec :: Spec
spec = do
  describe "html" $ do
    it "produces head with title + body" $ do
      render (html_ "My page title" (p_ "My page content")) `shouldBe` "<html><head><title>My page title</title></head><body><p>My page content</p></body></html>"

    it "escapes characters" $ do
      wrapTest (p_ "he\"llo & world") `shouldBe` wrap "<p>he&quot;llo &amp; world</p>"

    it "makes lists" $ do
      wrapTest (ul_ [p_ "a", p_ "b"]) `shouldBe` wrap "<ul><li><p>a</p></li><li><p>b</p></li></ul>"

    it "appends structures" $ do
      wrapTest (p_ "test" <> p_ "test") `shouldBe` wrap "<p>test</p><p>test</p>"
