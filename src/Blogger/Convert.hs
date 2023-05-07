module Blogger.Convert where

import Blogger.Env (Env (eBlogName, eStylesheetPath))
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)

convert :: Env -> String -> Markup.Document -> Html.Html
convert env t =
  let h = Html.title_ (eBlogName env <> " - " <> t) <> Html.stylesheet_ (eStylesheetPath env)
   in Html.html_ h . foldMap convertStructure
