module Blogger.CLI.Parser where

import Blogger.Env (Env (..))
import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath Env
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath OutputReplace
  deriving (Show)

type OutputReplace = Bool

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> output <*> replace
  where
    output =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file"
        )
    replace =
      switch
        ( long "replace"
            <> short 'r'
            <> help "Overwrite output"
        )

pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "DIRECTORY"
        <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIRECTORY"
        <> help "Output directory"
    )

pEnv :: Parser Env
pEnv =
  Env
    <$> strOption
      ( long "blog-name"
          <> short 'n'
          <> metavar "NAME"
          <> help "Name of blog"
      )
    <*> strOption
      ( long "stylesheet-path"
          <> short 's'
          <> metavar "FILE"
          <> help "Styles.css?"
      )

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
  info
    (helper <*> pConvertDir)
    (progDesc "Convert a markup dir to html")

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand =
  command "convert-dir" pConvertDirInfo

pOptions :: Parser Options
pOptions = subparser $ pConvertDirCommand <> pConvertSingleCommand
