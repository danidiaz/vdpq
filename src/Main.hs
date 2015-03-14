{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import BasePrelude
import MTLPrelude

import Data.Map.Strict
import qualified Data.Set as S
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Control.Concurrent.Async

import qualified Options.Applicative as O

import Network
import qualified Filesystem as F
import System.IO

import VDPQ.IO


defaultPlanFile :: String
defaultPlanFile = "plan.json"

type Errors r = Either Timeout (Either ResponseError r)

type Responses = Schema (Map String (Errors VDPResponse))

performQueries :: Int -> Seconds -> Plan -> IO Responses 
performQueries semsize seconds plan = do
        sem <- liftIO (newQSem semsize)
        names <- liftIO (newMVar S.empty)
        let decoratorFunc name = 
                itraverse .
                withConc sem .
                withLog names name .
                withTimeout seconds
            decoratorSchema = Schema
                decoratorFunc
        runConcurrently $
            decoratorSchema
            `apSchema`
            namesSchema
            `apSchema`
            executorSchema
            `traverseSchema`
            plan

diffReport :: Responses -> Responses -> [((String,String),[String])]
diffReport oldr newr =
    reportSchema (zipSchema `apSchema` oldr `apSchema` newr)
  where
    zipFunc = intersectionWith Pair
    zipSchema = Schema
        zipFunc 

data Command = 
    Example
  | Query FilePath String 
  | Report String
  | Cat
  | Collate String String
  | Diff
  | Pretty FilePath
  | Debug FilePath
  deriving (Show)   

parserInfo' :: O.ParserInfo Command  
parserInfo' = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser Command 
    parser' = (O.subparser . foldMap command') 
        [ ("example", "Generate example plan", pure Example)
        , ("query", "Perform queries and save the responses", queryP)
        , ("report", "Report on responses", reportP) 
        , ("cat", "Show set of responses", pure Cat) 
        , ("collate", "Collate two sets of responses", collateP) 
        , ("diff", "Compare two responses", pure Diff) 
        , ("pretty", "Print queries", prettyP) 
        , ("debug", "Debug queries", debugP) 
        ] 

    queryP = Query <$> planOpt <*> destFolderArg 
    prettyP = Pretty <$> planOpt
    debugP = Debug <$> planOpt
    reportP = Report <$> destFolderArg  
    collateP = Collate <$> destFolderArg <*> destFolderArg

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

withOops :: ExceptT String IO a -> IO ()
withOops = runExceptT >=> mapMOf_ _Left printErrAndExit
  where
    printErrAndExit err = hPutStrLn stderr err *> exitFailure

runnablePlan :: FilePath -> ExceptT String IO Plan  
runnablePlan planfile = defaultFillPlan <$> loadPlan (fromString planfile)

main :: IO ()
main = withSocketsDo $ do
    plan <- O.execParser parserInfo'
    case plan of
        Example -> BL.putStr (encodePretty examplePlan) 
        Query planfile folder -> withOops $ do
            plan' <- runnablePlan planfile
            let seconds = Seconds 7
            result <- liftIO $ performQueries 2 seconds plan' 
            let folder' = fromString folder
            tryAnyS $ F.createDirectory False folder'
            liftIO $ writeToFolder folder' result
        Report folder  -> withOops $ do
            r :: Responses <- liftIO $ readFromFolder (fromString folder)
            liftIO $ writeReport (reportSchema r)
        Collate folder1 folder2 -> withOops $ do
            r1 :: Responses <- liftIO $ readFromFolder (fromString folder1)
            r2 :: Responses <- liftIO $ readFromFolder (fromString folder2)
            liftIO $ writeReport (diffReport r1 r2)
        Pretty planfile -> withOops $ do
            plan' <- defaultFillPlan <$> loadPlan (fromString planfile)
            iforOf_ (vdp . ifolded) plan' $ \k q -> liftIO $ do
                print k
                let (schemaurl,dataurl) = buildVDPURLPair q
                print schemaurl
                print dataurl
        _ -> putStrLn "foo"
    return ()
