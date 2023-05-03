{-# LANGUAGE LambdaCase #-}

module Blogger.CLI where

import Blogger.Convert (process)
import System.Directory (doesFileExist)
import System.Environment as Env (getArgs)

run :: IO ()
run = do
  args <- Env.getArgs
  input <- getInput args
  output <- getOutputWriter args
  output $ process "test" input
  putStrLn "x"

getInput :: [String] -> IO String
getInput args =
  case args of
    [] -> getContents
    (inFile : _) -> do
      c <- doesFileExist inFile
      if c then readFile inFile else return ""

getOutputWriter :: [String] -> IO (String -> IO ())
getOutputWriter args =
  case args of
    [] -> pure putStrLn
    (_ : outFile : _) -> pure $ writeOut outFile
    _ -> error "messed"

writeOut :: String -> String -> IO ()
writeOut fp content = do
  exists <- doesFileExist fp
  let writeResult = writeFile fp content
  if exists
    then
      promptOverwrite
        >>= \case
          False -> putStrLn "Aborted"
          True -> writeResult
    else writeResult

promptOverwrite :: IO Bool
promptOverwrite = do
  putStrLn "File exists. Overwrite?"
  res <- getLine
  case res of
    "y" -> return $ True
    _ -> return False
