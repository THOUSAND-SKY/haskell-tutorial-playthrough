module Blogger.CLI where

import Blogger.CLI.Parser
import Blogger.Convert (convert)
import Blogger.Env (Env, defaultEnv)
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup
import Control.Exception (bracket)
import Control.Monad.Trans.Reader (withReader)
import Data.Bool (bool)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: String -> String -> String
process t = Html.render . convert defaultEnv t . Markup.parse

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
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
      let (title, iHandle) = inputWriter input
      let oHandle = outputWriter output

      bracket iHandle hClose $ \i ->
        bracket oHandle hClose $ \o ->
          convertSingle title i o
    ConvertDir input output env ->
      convertDirectory env input output

inputWriter :: SingleInput -> (String, IO Handle)
inputWriter input =
  case input of
    Stdin ->
      ("", pure stdin)
    InputFile f ->
      (f, openFile f ReadMode)

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
