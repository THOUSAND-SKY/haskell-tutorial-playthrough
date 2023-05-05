-- | Process multiple files and convert directories
module Blogger.Directory
  ( applyIoOnList,
  )
where

-- convertDirectory,
-- buildIndex,

import Blogger.Convert (convert, convertStructure)
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup
import Control.Exception (SomeException (..), catch, displayException, try)
import Control.Monad (void, when)
import Data.Bifunctor (first)
import Data.List (partition)
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
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
-- convertDirectory :: FilePath -> FilePath -> IO ()
-- convertDirectory inputDir outputDir = do
--   DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
--   createOutputDirectoryOrExit outputDir
--   let outputHtmls = txtsToRenderedHtml filesToProcess
--   copyFiles outputDir filesToCopy
--   writeFiles outputDir outputHtmls
--   putStrLn "Done."

-- -- | The relevant directory content for our application
-- data DirContents = DirContents
--   { -- | File paths and their content
--     dcFilesToProcess :: [(FilePath, String)],
--     -- | Other file paths, to be copied directly
--     dcFilesToCopy :: [FilePath]
--   }

-- -- | Returns the directory content
-- getDirFilesAndContent :: FilePath -> IO DirContents
-- getDirFilesAndContent inputDir = do
--   files <- map (inputDir </>) <$> listDirectory inputDir
--   let (txtFiles, otherFiles) =
--         partition ((== ".txt") . takeExtension) files
--   txtFilesAndContent <-
--     applyIoOnList readFile txtFiles >>= filterAndReportFailures
--   pure $
--     DirContents
--       { dcFilesToProcess = txtFilesAndContent,
--         dcFilesToCopy = otherFiles
--       }

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

-- case as of
--   (x:xs) -> do
--     result <- f x
--     return []
--     -- f x : applyIoOnList xs
--   [] ->
--     return []
