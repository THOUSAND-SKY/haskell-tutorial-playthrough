{-# LANGUAGE OverloadedStrings #-}

module Blogger.HtmlSpec where

import Blogger.Html.Html (html_, p_, render, title_, txt_, ul_)
import Test.Hspec

wrap s = "<html><head><title></title></head><body>" <> s <> "</body></html>"

wrapTest = render . html_ (title_ "")

spec :: Spec
spec = do
  describe "html" $ do
    it "produces head with title + body" $ do
      render (html_ (title_ "My page title") (p_ $ txt_ "My page content")) `shouldBe` "<html><head><title>My page title</title></head><body><p>My page content</p></body></html>"

    it "escapes characters" $ do
      wrapTest (p_ (txt_ "he\"llo & world")) `shouldBe` wrap "<p>he&quot;llo &amp; world</p>"

    it "makes lists" $ do
      wrapTest (ul_ [p_ (txt_ "a"), p_ (txt_ "b")]) `shouldBe` wrap "<ul><li><p>a</p></li><li><p>b</p></li></ul>"

    it "appends structures" $ do
      wrapTest (p_ (txt_ "test") <> p_ (txt_ "test")) `shouldBe` wrap "<p>test</p><p>test</p>"
