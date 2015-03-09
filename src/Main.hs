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
import qualified Data.Set as S
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T

import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Concurrent.MVar

import Pipes
--import qualified Pipes.ByteString as B
import Pipes.Aeson (encodeObject)

import qualified Options.Applicative as O

import Network

import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F

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

type Errors r = Either Timeout (Either ResponseError r)

type Responses = Schema' (Map String)
                         (Errors VDPResponse)

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

timeLimit :: Seconds
timeLimit = Seconds 10

main :: IO ()
main = withSocketsDo $ do
    plan <- O.execParser parserInfo'
    case plan of
        Example -> BL.putStr (encodePretty examplePlan) 
        Query folder planfile -> do
            result <- runExceptT $ do
                plan <- defaultFillPlan <$> loadPlan (fromString planfile)
                sem <- liftIO (newQSem 2)
                names <- liftIO (newMVar S.empty)
                let seconds = Seconds 7
                    decorator name = 
                        withConc sem .
                        withLog names name .
                        withTimeout seconds
                    -- this works thanks to let-polymorphism
                    decoratedExecutor = 
                        (Schema
                            decorator)
                        `apSchema`
                        namesSchema
                        `apSchema`
                        basicExecutor
                result <- (liftIO . runConcurrently)
                    (traverseSchema decoratedExecutor plan)
                let resultMap = view vdp result 
                liftIO (print resultMap)
                tryAnyS (F.createDirectory False (fromString folder))
            case result of
                Left msg -> putStrLn msg
                Right _ -> return ()
        Pretty planfile -> do
            result <- runExceptT $ do
                plan <- defaultFillPlan <$> loadPlan (fromString planfile)
                iforOf_ (vdp . ifolded) plan $ \k q -> liftIO $ do
                    print k
                    let (schemaurl,dataurl) = buildVDPURLPair q
                    print schemaurl
                    print dataurl
            case result of
                Left msg -> putStrLn msg
                Right _ -> return ()
        _ -> putStrLn "foo"
    return ()
