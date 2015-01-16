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

complete :: O.Parser a -> String -> O.ParserInfo a
complete p desc = O.info (O.helper <*> p) 
                         (mconcat [O.fullDesc, O.progDesc desc])

theParser :: O.Parser Command 
theParser = O.subparser . mconcat . fmap mksubc $
        [ ("example", pure Example, "Generate example plan")
        , ("query", pure Query, "Perform queries") 
        , ("report", pure Report, "Report on responses") 
        , ("cat", pure Cat, "Show set of responses") 
        , ("collate",pure Collate, "Collate two sets of responses") 
        , ("diff", pure Diff, "Compare two responses") 
        , ("pretty", pure Pretty, "Print queries") 
        ] 
  where 
    mksubc (name,p,desc) = O.command name (complete p desc)

main :: IO ()
main = do
    O.execParser (complete theParser "This is the main prog desc")
    print "" 
