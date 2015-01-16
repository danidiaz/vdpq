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

complete p desc = O.info (O.helper <*> p) 
                         (O.fullDesc <> O.progDesc desc)

theParser :: O.Parser Command 
theParser = (O.subparser . mconcat)
    [ O.command "example" (complete (pure Example) "Generate example plan")
    , O.command "query" (complete (pure Query) "Perform queries") ]

main :: IO ()
main = do
    O.execParser (complete theParser "This is the main prog desc")
    print "" 
