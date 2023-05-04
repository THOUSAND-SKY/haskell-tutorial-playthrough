module Main (main) where

import Blogger.CLI (run)
-- main :: IO ()
-- main = run

import Blogger.CLI.Parser (pOptions)
import Options.Applicative

main :: IO ()
main = print =<< execParser opts
  where
    opts =
      info
        (pOptions <**> helper)
        -- I wonder how to omit these?
        ( fullDesc
            <> header "hs-blog-gen - a static blog generator"
            <> progDesc "Convert markup files or directories to html"
        )
