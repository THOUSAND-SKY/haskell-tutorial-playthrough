-- | Process multiple files and convert directories
module Blogger.Directory
  ( applyIoOnList,
    filterAndReportFailures,
  )
where

-- convertDirectory,
-- buildIndex,

import Blogger.Convert (convert, convertStructure)
import Blogger.Html.Html as Html
import Blogger.Markup.Markup as Markup
import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
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

-- filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
-- filterAndReportFailures xs = do
--   let printErr err = hPutStrLn stderr ("Error: " ++ err)
--   let ys =
--         map
--           ( \(x, y) ->
--               case y of
--                 Left err -> printErr err $> Nothing
--                 Right z -> return $ Just (x, z)
--           )
--           xs
--   zs <- sequence ys
--   return (catMaybes zs)

-- let (errs, vals) = partition (isLeft . snd) xs
-- for_ errs $ \(_, v) ->
--   case v of
--     Left err ->
--     Right _ -> return ()
-- return $ for vals $ \(a, b) ->
--   case b of
--     Right

-- for_ errs $ \(_, err) ->
-- traverse $ \(a, b) -> do
--   return []
-- case b of
--   Left err -> hPutStrLn stderr $ "Error: " ++ err
--   Right val -> val
