{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BasePrelude
import MTLPrelude

import qualified Options.Applicative as O

data Command = 
    Example
  | Query
  | Report
  | Cat
  | Collate
  | Diff
  | Pretty 
  deriving (Show)   

info' :: O.Parser a -> String -> O.ParserInfo a
info' p desc = O.info 
    (O.helper <*> p) 
    (mconcat [O.fullDesc, O.progDesc desc])

theParser :: O.Parser Command 
theParser = (O.subparser . mconcat . fmap command') 
    [ ("example", pure Example, "Generate example plan")
    , ("query", pure Query, "Perform queries") 
    , ("report", pure Report, "Report on responses") 
    , ("cat", pure Cat, "Show set of responses") 
    , ("collate",pure Collate, "Collate two sets of responses") 
    , ("diff", pure Diff, "Compare two responses") 
    , ("pretty", pure Pretty, "Print queries") 
    ] 
  where 
    command' (name,p,desc) = O.command name (info' p desc)

main :: IO ()
main = do
    O.execParser (info' theParser "This is the main prog desc")
    print "" 
