{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module VDPQ.Plan where

import Data.Char
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Identity

import Control.Lens

import GHC.Generics

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

data VDPQuery' f = VDPQuery'
    {
        _targetVDP :: String         
    ,   _viewName :: String
    ,   _whereClause :: Maybe String
    ,   _tvdp :: f TargetVDP
    }
    deriving (Generic)

deriving instance Show (f TargetVDP) => Show (VDPQuery' f) 

instance FromJSON (VDPQuery' Maybe) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON (VDPQuery' Maybe) where
    toJSON = genericToJSON aesonOptions

vdpQueryDefault :: TargetVDP -> VDPQuery' Maybe -> VDPQuery' Identity 
vdpQueryDefault dt r =
    let t' = maybe (Identity dt) Identity (_tvdp r) in r{ _tvdp = t'}

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

data Query =
    VDPQuery (VDPQuery' Maybe)
  | Foo String
    deriving (Show,Generic)

instance FromJSON Query where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Query where
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

$(makeLenses ''TargetVDP)
$(makeLenses ''VDPQuery')
$(makeLenses ''Plan)
$(makeLenses ''Targets)
$(makePrisms ''Query)
