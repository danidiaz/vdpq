{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module VDPQ.Plan where

import Data.Char
import Data.Map
import Data.Aeson
import Data.Aeson.Types

import Control.Lens

import GHC.Generics

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
        vdpTargets :: Map String TargetVDP
    } 
    deriving (Show,Generic)

instance FromJSON Targets

instance ToJSON Targets

data TargetVDP = TargetVDP 
    { host :: String
    , port :: Int
    , login :: String
    , password :: String
    , database :: String 
    }
    deriving (Show,Generic)

instance FromJSON TargetVDP

instance ToJSON TargetVDP

data Named v = Named
    { 
        name :: String
    ,   value :: v
    } 
    deriving (Show,Generic,Functor)

instance FromJSON v => FromJSON (Named v)

instance ToJSON v => ToJSON (Named v)

data Query =
    VdpQuery VdpQuery'
  | Foo String
    deriving (Show,Generic)

aesonOptions :: Options
aesonOptions = defaultOptions 
    { sumEncoding = ObjectWithSingleField 
    , constructorTagModifier = over _head toLower
    , omitNothingFields = True
    }

instance FromJSON Query where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Query where
    toJSON = genericToJSON aesonOptions


data VdpQuery' = VdpQuery'
    {
        targetVDP :: String         
    ,   viewName :: String
    ,   whereClause :: Maybe String
    }
    deriving (Show,Generic)

instance FromJSON VdpQuery' where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VdpQuery' where
    toJSON = genericToJSON aesonOptions
