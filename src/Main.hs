{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE OverloadedLists #-}
--{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import BasePrelude
import MTLPrelude

import Data.Map
import Data.Aeson
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy as BL

import Pipes
--import qualified Pipes.ByteString as B
import Pipes.Aeson (encodeObject)


import qualified Options.Applicative as O

import GHC.Generics
--import GHC.Exts (IsList(..))

data Command = 
    Example
  | Query
  | Report
  | Cat
  | Collate
  | Diff
  | Pretty 
  deriving (Show)   

data Plan = Plan 
    {
        targets :: Targets
    ,   queries :: [Named Query]
    } 
    deriving (Show,Generic)

instance FromJSON Plan
instance ToJSON Plan

data Targets = Targets
    {            
        vdpTargets :: Map String String
    } 
    deriving (Show,Generic)

instance FromJSON Targets
instance ToJSON Targets

data Named v = Named
    { 
        name :: String
    ,   value :: v
    } 
    deriving (Show,Generic,Functor)

instance FromJSON v => FromJSON (Named v)
instance ToJSON v => ToJSON (Named v)

type Query = String

example :: Plan
example = Plan 
    (Targets 
        (Data.Map.fromList [("vdp","foo")])) 
    [Named "q1" "select * from foo"]

--exampleObject :: Object
--exampleObject = case toJSON example of 
--    Object o -> o
--    _ -> error "should never happen"


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
            
    command' (cname,p,desc) = O.command cname (info' p desc)


main :: IO ()
main = do
    plan <- O.execParser parserInfo'
    case plan of
        Example -> BL.putStr (encodePretty example) 
        _ -> putStrLn "foo"
