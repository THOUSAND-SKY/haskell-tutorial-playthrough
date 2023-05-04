module Blogger.CLI where

import Blogger.CLI.Parser
import Blogger.Convert (convert)
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup
import Data.Bool (bool)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process t = Html.render . convert t . Markup.parse

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

parseOpts :: IO Options
parseOpts =
  execParser opts
  where
    opts =
      info
        (pOptions <**> helper)
        -- I wonder how to omit these?
        ( fullDesc
            <> header "hs-blog-gen - a static blog generator"
            <> progDesc "Convert markup files or directories to html"
        )

run :: IO ()
run = do
  opts <- parseOpts
  case opts of
    ConvertSingle input output -> do
      (title, iHandle) <- inputWriter input
      oHandle <- outputWriter output

      convertSingle title iHandle oHandle

      hClose iHandle
      hClose oHandle
    ConvertDir _ _ ->
      error "err"

inputWriter :: SingleInput -> IO (String, Handle)
inputWriter input =
  case input of
    Stdin ->
      return ("", stdin)
    InputFile f ->
      -- same as
      -- openFile f ReadMode >>= \x -> return (f,x)
      (,) f <$> openFile f ReadMode

outputWriter :: SingleOutput -> IO Handle
outputWriter output =
  case output of
    Stdout ->
      return stdout
    OutputFile f replace -> do
      exists <- doesFileExist f
      let write = openFile f WriteMode
      if exists && not replace
        then confirm >>= bool write exitFailure
        else write

confirm :: IO Bool
confirm = do
  putStrLn "File exists. Overwrite?"
  res <- getLine
  case res of
    "y" -> return True
    _ -> return False
