{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module VDPQ.Plan where

import Data.Char
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.Types

import Control.Lens

import GHC.Generics

data Plan = Plan 
    {
        _targets :: Targets
    ,   _queries :: Map String Query
    } 
    deriving (Show,Generic)

instance FromJSON Plan where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan where
    toJSON = genericToJSON aesonOptions

data Targets = Targets
    {            
        _vdpTargets :: Map String TargetVDP
    } 
    deriving (Show,Generic)

instance FromJSON Targets where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Targets where
    toJSON = genericToJSON aesonOptions

data TargetVDP = TargetVDP 
    { _host :: String
    , _port :: Int
    , _login :: String
    , _password :: String
    , _database :: String 
    }
    deriving (Show,Generic)

instance FromJSON TargetVDP where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON TargetVDP where
    toJSON = genericToJSON aesonOptions

data Query =
    VDPQuery VDPQuery'
  | Foo String
    deriving (Show,Generic)

instance FromJSON Query where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Query where
    toJSON = genericToJSON aesonOptions

data VDPQuery' = VDPQuery'
    {
        _targetVDP :: String         
    ,   _viewName :: String
    ,   _whereClause :: Maybe String
    }
    deriving (Show,Generic)

instance FromJSON VDPQuery' where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VDPQuery' where
    toJSON = genericToJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions 
    { sumEncoding = ObjectWithSingleField 
    , fieldLabelModifier = overload . tail
    , constructorTagModifier = overload 
    , omitNothingFields = True
    }
  where
      overload name = head (filter (isMatch name) overloaded ++ [name]) 
      isMatch name prefix = isPrefixOf (fmap toLower prefix) (fmap toLower name)

      overloaded = ["VDP"]

$(makeLenses ''Plan)
$(makeLenses ''Targets)
$(makeLenses ''TargetVDP)
$(makePrisms ''Query)
$(makeLenses ''VDPQuery')
