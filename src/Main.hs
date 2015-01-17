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


parserInfo' :: O.ParserInfo Command  
parserInfo' = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser Command 
    parser' = (O.subparser . foldMap command') 
        [ ("example", pure Example, "Generate example plan")
        , ("query", pure Query, "Perform queries") 
        , ("report", pure Report, "Report on responses") 
        , ("cat", pure Cat, "Show set of responses") 
        , ("collate",pure Collate, "Collate two sets of responses") 
        , ("diff", pure Diff, "Compare two responses") 
        , ("pretty", pure Pretty, "Print queries") 
        ] 

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info 
        (O.helper <*> p) 
        (O.fullDesc <> O.progDesc desc)
            
    command' (name,p,desc) = O.command name (info' p desc)


main :: IO ()
main = do
    spec <- O.execParser parserInfo'
    print spec
