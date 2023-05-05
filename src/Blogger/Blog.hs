module Blogger.Blog where

import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex paths =
  Html.html_ "Blog" $
    Html.h1_ (Html.a_ "index.html" $ Html.txt_ "Blog Index")
      <> Html.h_ 2 (Html.txt_ "Posts")
      <> links paths

links :: [(FilePath, Markup.Document)] -> Html.Structure
links paths =
  let linkify (fp, markup) = Html.a_ fp $ txt_ $ summary markup

      summary :: Markup.Document -> String
      summary markup =
        case markup of
          (Markup.Heading 1 x : _) -> x
          _ -> "No content"
   in Html.ul_ (map linkify paths)
