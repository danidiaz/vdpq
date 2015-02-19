{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverloadedLists #-}
--{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import BasePrelude
import MTLPrelude

import Data.Char
import Data.Map
--import qualified Data.Attoparsec as A
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Control.Concurrent.Async

import Pipes
--import qualified Pipes.ByteString as B
import Pipes.Aeson (encodeObject)

import qualified Options.Applicative as O

import GHC.Generics
--import GHC.Exts (IsList(..))

import System.FilePath 
import System.Directory

import VDPQ.Plan


data Command = 
    Example
  | Query String FilePath
  | Report
  | Cat
  | Collate
  | Diff
  | Pretty 
  deriving (Show)   

defaultVDPServer :: VDPServer
defaultVDPServer = VDPServer "localhost" 9999 "admin" "admin" "admin"

defaultTemplateName :: String
defaultTemplateName = "_template"

examplePlan :: Plan
examplePlan = Plan 
    (Data.Map.fromList
        [ (defaultTemplateName, VDPQuery "fooview" (Just "where 1 = 3") (Just defaultVDPServer))
        , ("q1", VDPQuery "fooview" (Just "where 1 = 3") Nothing)
        , ("q2", VDPQuery "barview" Nothing Nothing) 
        ]
    )

vdpQueryDefault :: VDPServer -> VDPQuery Maybe -> VDPQuery Identity 
vdpQueryDefault dt = over targetVDP (Identity . maybe dt id)

fillVDPTargets :: Map String (VDPQuery Maybe) -> Map String (VDPQuery Identity) 
fillVDPTargets qs =
    let altServer = preview (ix defaultTemplateName.targetVDP.folded) qs 
        defaultVDPServer' = maybe defaultVDPServer id altServer
        qs' = filterWithKey (\k _ -> head k == '_') qs
    in  fmap (vdpQueryDefault defaultVDPServer') qs'

parserInfo' :: O.ParserInfo Command  
parserInfo' = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser Command 
    parser' = (O.subparser . foldMap command') 
        [ ("example", pure Example, "Generate examplePlan plan")
        , ("query", queryP, "Perform queries and save the responses") 
        , ("report", pure Report, "Report on responses") 
        , ("cat", pure Cat, "Show set of responses") 
        , ("collate",pure Collate, "Collate two sets of responses") 
        , ("diff", pure Diff, "Compare two responses") 
        , ("pretty", pure Pretty, "Print queries") 
        ] 

    destFolderP = O.strArgument 
        (mconcat 
            [ O.help "Destination folder for the results."
            , O.metavar "FOLDER" ])

    planP = 
        O.strOption
            (mconcat 
                [ O.help "Query plan file."
                , O.value "plan.json"
                , O.showDefault
                , O.long "plan"
                , O.metavar "PLAN" ])


    queryP = Query <$> destFolderP <*> planP

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info 
        (O.helper <*> p) 
        (O.fullDesc <> O.progDesc desc)
            
    command' (cname,p,desc) = O.command cname (info' p desc)


loadJSON :: FromJSON a => FilePath -> ExceptT String IO a
loadJSON path = 
    withExceptT ("Loading JSON: "++) $ do
        bytes <- tryAsync (B.readFile path)
        ExceptT (return (eitherDecode (BL.fromStrict bytes)))

tryAsync :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAsync action = withExceptT show (ExceptT (liftIO (withAsync action waitCatch)))

main :: IO ()
main = do
    plan <- O.execParser parserInfo'
    case plan of
        Example -> BL.putStr (encodePretty examplePlan) 
        Query folder planfile -> do
            result <- runExceptT $ do
                plan :: Plan <- loadJSON planfile        
                tryAsync (createDirectory folder)
            --mapMOf_ _Left putStrLn result
            case result of
                Left msg -> putStrLn msg
                Right _ -> return ()
        _ -> putStrLn "foo"
