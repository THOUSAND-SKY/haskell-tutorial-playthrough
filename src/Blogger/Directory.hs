-- | Process multiple files and convert directories
module Blogger.Directory
  ( applyIoOnList,
    filterAndReportFailures,
    txtsToRenderedHtml,
  )
where

-- convertDirectory,
-- buildIndex,

import Blogger.Blog (buildIndex)
import Blogger.Convert (convert, convertStructure)
import Blogger.Env (AppEnv, Env (eBlogName))
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup
import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Control.Monad.Trans.Reader (Reader, ask)
import Data.Bifunctor (Bifunctor (second))
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (partition)
import Data.Maybe (catMaybes, maybeToList)
import Data.Traversable (for)
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath
  ( takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)

-- | Copy files from one directory to another, converting '.txt' files to
-- '.html' files in the process. Recording unsuccessful reads and writes to stderr.

-- -- May throw an exception on output directory creation.
-- convertDirectory :: FilePath -> FilePath -> IO ()
-- convertDirectory inputDir outputDir = do
--   DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
--   createOutputDirectoryOrExit outputDir
--   let outputHtmls = txtsToRenderedHtml filesToProcess
--   copyFiles outputDir filesToCopy
--   writeFiles outputDir outputHtmls
--   putStrLn "Done."

-- | The relevant directory content for our application
data DirContents = DirContents
  { -- | File paths and their content
    dcFilesToProcess :: [(FilePath, String)],
    -- | Other file paths, to be copied directly
    dcFilesToCopy :: [FilePath]
  }

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) =
        partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $
    DirContents
      { dcFilesToProcess = txtFilesAndContent,
        dcFilesToCopy = otherFiles
      }

-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
            pure $ Left (displayException e)
        )
    pure (input, maybeResult)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

whenIO :: IO Bool -> IO () -> IO ()
whenIO a b = a >>= flip when b

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else pure True
  when create (createDirectory dir)
  pure create

confirm :: String -> IO Bool
confirm prompt = do
  putStrLn prompt
  res <- getLine
  case res of
    "y" -> return True
    _ -> return False

-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> AppEnv [(FilePath, String)]
txtsToRenderedHtml xs = do
  let replaceFileExt name = takeBaseName name <.> "html"
  let entry (file, content) = (replaceFileExt file, Markup.parse content)
  let outputs = map entry xs
  env <- ask
  let convertFile (n, c) = (n, convert env n c)
  newOutputs <- buildIndex outputs
  let index = (eBlogName env, newOutputs)
  return $ map (fmap Html.render) $ index : map convertFile outputs

applyIoOnListReportErrors :: (a -> IO b) -> [a] -> IO [(a, b)]
applyIoOnListReportErrors a b =
  applyIoOnList a b >>= filterAndReportFailures

-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnListReportErrors copyFromTo files

-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) =
        writeFile (outputDir </> file) content
  void $ applyIoOnListReportErrors writeFileContent files
