-- | Process multiple files and convert directories
module Blogger.Directory
  ( applyIoOnList,
  )
where

-- convertDirectory,
-- buildIndex,

import Blogger.Convert (convert, convertStructure)
import qualified Blogger.Html.Html as Html
import qualified Blogger.Markup.Markup as Markup
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


applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList f =
  traverse mapper
  where
    -- mapper :: a -> IO (a, Either String b)
    mapper a =
      let fn b = do
            res <- try $ f b
            -- How would I correctly type this one??
            -- It fails on the `b` because that's not bound to the above `b`.
            -- return $ first displayException (res :: Either SomeException b)
            case res of
              Left e -> return $ Left $ displayException (e :: SomeException)
              Right val -> return $ Right val
       in (,) a <$> fn a

-- case as of
--   (x:xs) -> do
--     result <- f x
--     return []
--     -- f x : applyIoOnList xs
--   [] ->
--     return []
