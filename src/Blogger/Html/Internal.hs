module Blogger.Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (Structure a) <> (Structure b) = Structure $ a <> b

instance Monoid Structure where
  mempty = empty_

empty_ :: Structure
empty_ = Structure ""

p_ :: String -> Structure
p_ = tag "p"

h1_ :: String -> Structure
h1_ = tag "h1"

h_ :: Natural -> String -> Structure
h_ n = tag ("h" <> show n)

li_ :: String -> Structure
li_ = tag "li"

listing :: String -> [Structure] -> Structure
listing t =
  Structure . el t . concatMap (el "li" . getStructureString)

ul_ :: [Structure] -> Structure
ul_ =
  listing "ul"

ol_ :: [Structure] -> Structure
ol_ =
  listing "ol"

code_ :: String -> Structure
code_ = tag "pre"

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html $
    el "html" $
      el "head" (el "title" title)
        <> el "body" content

render :: Html -> String
render (Html s) = s

concatStructure :: [Structure] -> Structure
concatStructure c =
  case c of
    [] -> empty_
    (x : xs) -> x <> mconcat xs

getStructureString :: Structure -> String
getStructureString (Structure s) = s

tag :: String -> String -> Structure
tag t c = Structure . el t $ escape c

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
