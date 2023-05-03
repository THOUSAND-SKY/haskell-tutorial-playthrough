module Blogger.Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure $ a <> b

p_ :: String -> Structure
p_ = tag "p"

h1_ :: String -> Structure
h1_ = tag "h1"

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
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
