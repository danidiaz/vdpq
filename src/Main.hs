{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverloadedLists #-}
--{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import BasePrelude
import MTLPrelude

import Data.Map
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Control.Concurrent.Async

import Pipes
--import qualified Pipes.ByteString as B
import Pipes.Aeson (encodeObject)

import qualified Options.Applicative as O

import System.Directory

import VDPQ.IO

data Command = 
    Example
  | Query String FilePath
  | Report
  | Cat
  | Collate
  | Diff
  | Pretty FilePath
  deriving (Show)   

defaultPlanFile :: String
defaultPlanFile = "plan.json"

parserInfo' :: O.ParserInfo Command  
parserInfo' = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser Command 
    parser' = (O.subparser . foldMap command') 
        [ ("example", "Generate example plan", pure Example)
        , ("query", "Perform queries and save the responses", queryP)
        , ("report", "Report on responses", pure Report) 
        , ("cat", "Show set of responses", pure Cat) 
        , ("collate", "Collate two sets of responses", pure Collate) 
        , ("diff", "Compare two responses", pure Diff) 
        , ("pretty", "Print queries", prettyP) 
        ] 

    queryP = Query <$> destFolderArg <*> planOpt
    prettyP = Pretty <$> planOpt

    destFolderArg = O.strArgument 
        (mconcat 
            [ O.help "Destination folder for the results."
            , O.metavar "FOLDER" ])

    planOpt = O.strOption
        (mconcat 
            [ O.help "Query plan file."
            , O.value defaultPlanFile
            , O.showDefault
            , O.long "plan"
            , O.metavar "PLAN" ])

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info 
        (O.helper <*> p) 
        (O.fullDesc <> O.progDesc desc)
            
    command' (cmdName,desc,parser) = 
        O.command cmdName (info' parser desc)


main :: IO ()
main = do
    plan <- O.execParser parserInfo'
    case plan of
        Example -> BL.putStr (encodePretty examplePlan) 
        Query folder planfile -> do
            result <- runExceptT $ do
                plan <- defaultFillVDPTargets . _vdp <$> loadPlan planfile
                tryAsync (createDirectory folder)
            --mapMOf_ _Left putStrLn result
            case result of
                Left msg -> putStrLn msg
                Right _ -> return ()
        _ -> putStrLn "foo"
