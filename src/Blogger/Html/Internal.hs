module Blogger.Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype HeadStructure = HeadStructure Structure

instance Semigroup HeadStructure where
  (HeadStructure a) <> (HeadStructure b) = HeadStructure $ a <> b

instance Monoid HeadStructure where
  mempty = HeadStructure empty_

data Structure = Content String | Nested Structure
  deriving (Eq, Show)

type Title = String

instance Semigroup Structure where
  (Content a) <> (Content b) = Content $ a <> b
  (Nested a) <> (Content b) = Content $ getStructureString a <> b
  (Content a) <> (Nested b) = Content $ a <> getStructureString b
  Nested a <> Nested b = a <> b

instance Monoid Structure where
  mempty = empty_

empty_ :: Structure
empty_ = Content ""

txt_ :: String -> Structure
txt_ = children . escape

img_ :: FilePath -> String -> Structure
img_ src alt =
  children $
    "<img src=\"" <> escape src <> "\" alt=\"" <> escape alt <> "\">"

a_ :: String -> Structure -> Structure
a_ href content =
  children $
    "<a href=\"" <> escape href <> "\">" <> getStructureString content <> "</a>"

p_ :: Structure -> Structure
p_ = tag "p"

h1_ :: Structure -> Structure
h1_ = tag "h1"

h_ :: Natural -> Structure -> Structure
h_ n = tag ("h" <> show n)

li_ :: Structure -> Structure
li_ = tag "li"

listing :: String -> [Structure] -> Structure
listing t =
  children . el t . concatMap (el "li" . getStructureString)

ul_ :: [Structure] -> Structure
ul_ =
  listing "ul"

ol_ :: [Structure] -> Structure
ol_ =
  listing "ol"

code_ :: String -> Structure
code_ = tag "pre" . txt_

html_ :: HeadStructure -> Structure -> Html
html_ (HeadStructure docHead) content =
  Html $
    el "html" $
      el "head" (getStructureString docHead)
        <> el "body" (getStructureString content)

title_ :: String -> HeadStructure
title_ = headEl . el "title"

-- vulnerable. don't care.
stylesheet_ :: String -> HeadStructure
stylesheet_ s =
  headEl $
    "<style src=\"" <> escape s <> "\"></style>"

meta_ :: String -> String -> HeadStructure
meta_ name content =
  headEl $
    "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

render :: Html -> String
render (Html s) = s

concatStructure :: [Structure] -> Structure
concatStructure c =
  case c of
    [] -> empty_
    (x : xs) -> x <> mconcat xs

getStructureString :: Structure -> String
getStructureString s =
  case s of
    Content c -> c
    Nested c -> getStructureString c

tag :: String -> Structure -> Structure
tag t c =
  case c of
    Content str -> children $ el t $ escape str
    Nested nc -> children $ el t $ getStructureString nc

-- tag t c = Structure . el t $ getStructureString c

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

el :: String -> String -> String
el t content = "<" <> t <> ">" <> content <> "</" <> t <> ">"

headEl :: String -> HeadStructure
headEl = HeadStructure . Content

children :: String -> Structure
children = Nested . Content
