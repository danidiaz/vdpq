{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module VDPQ.Types where

import Data.Map (Map)
import Data.Monoid
import Data.Aeson
import Data.Aeson.Types

import Control.Applicative
import Control.Lens

import GHC.Generics


aesonOptions :: Options
aesonOptions = defaultOptions 
    { sumEncoding = ObjectWithSingleField 
    , fieldLabelModifier = tail
    , omitNothingFields = True
    }


data VDPServer = VDPServer 
    { _vdpHost :: String
    , _vdpPort :: Int
    , _vdpLogin :: String
    , _vdpPassword :: String
    , _vdpDatabase :: String 
    }
    deriving (Show,Generic)

instance FromJSON VDPServer where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VDPServer where
    toJSON = genericToJSON aesonOptions

$(makeLenses ''VDPServer)


data VDPQuery f = VDPQuery
    {
        _viewName :: String
    ,   _whereClause :: Maybe String
    ,   _targetVDP :: f VDPServer
    }
    deriving (Generic)

deriving instance Show (f VDPServer) => Show (VDPQuery f) 

$(makeLenses ''VDPQuery)

instance FromJSON (VDPQuery Maybe) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON (VDPQuery Maybe) where
    toJSON = genericToJSON aesonOptions


data Schema a = Schema
    {
        _vdp :: a
    } 
    deriving (Generic)

$(makeLenses ''Schema)

type Schema' f a = Schema (f a)

-- Boilerplate time !!!!!
traverseSchema :: (Applicative f, TraversableWithIndex i t) 
               => Schema (i -> a -> f a')
               -> Schema' t a
               -> f (Schema' t a')
traverseSchema (Schema fa) (Schema ta) = Schema <$> itraverse fa ta 

apSchema :: Schema (a -> a')
         -> Schema a
         -> Schema a'
apSchema (Schema fa) (Schema a) = Schema (fa a)

foldMapSchema :: (Monoid m, FoldableWithIndex i t) 
              => Schema (i -> a -> m)
              -> Schema' t a
              -> m
foldMapSchema (Schema fa) (Schema a) = ifoldMap fa a 

namesSchema :: Schema String
namesSchema = Schema "vdp"
--

type Plan_ = Schema' (Map String) (VDPQuery Maybe)

instance FromJSON Plan_ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan_ where
    toJSON = genericToJSON aesonOptions

type Plan = Schema' (Map String) (VDPQuery Identity)

