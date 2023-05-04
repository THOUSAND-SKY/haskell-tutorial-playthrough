module Blogger.Html.Internal where

import Numeric.Natural

newtype Html = Html String

data Structure = Content String | Nested Structure

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

html_ :: Title -> Structure -> Html
html_ title content =
  Html $
    el "html" $
      el "head" (el "title" title)
        <> el "body" (getStructureString content)

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

-- Structure s_ -> getStructureString s_

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

children :: String -> Structure
children = Nested . Content
