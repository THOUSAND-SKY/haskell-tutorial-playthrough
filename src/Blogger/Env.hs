module Blogger.Env where

import Control.Monad.Trans.Reader

type AppEnv = Reader Env

data Env = Env
  { eBlogName :: String,
    eStylesheetPath :: FilePath
  }
  deriving (Show)

defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css"

blankEnv :: Env
blankEnv = Env "" ""
