{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module VDPQ.Types where

import Data.Map (Map)
import Data.Monoid
import Data.Aeson
import Data.String
import Data.Typeable
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


data VDPResponse = VDPResponse 
    {
        _schema :: Value 
    ,   _data :: Value
    }
    deriving (Show,Eq,Typeable)


$(makeLenses ''VDPResponse)


newtype ResponseError = ResponseError String deriving (Show,Eq,Typeable)

instance IsString ResponseError where
    fromString = ResponseError


data Schema a = Schema
    {
        _vdp :: a
    } 
    deriving (Generic, Show)

$(makeLenses ''Schema)


-- Boilerplate time !!!!!
uniformSchema :: a -> Schema a 
uniformSchema a = Schema a

traverseSchema :: (Applicative f) 
               => Schema (a -> f a')
               -> Schema a
               -> f (Schema a')
traverseSchema (Schema fa) (Schema ta) = Schema <$> fa ta

apSchema :: Schema (a -> a')
         -> Schema a
         -> Schema a'
apSchema (Schema fa) (Schema a) = Schema (fa a)

foldMapSchema :: (Monoid m) 
              => Schema (a -> m)
              -> Schema a
              -> m
foldMapSchema (Schema fa) (Schema a) = fa a 

namesSchema :: Schema String
namesSchema = Schema "vdp"
-- boilerplate end.

type Plan_ = Schema (Map String (VDPQuery Maybe))

instance FromJSON Plan_ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan_ where
    toJSON = genericToJSON aesonOptions

type Plan = Schema (Map String (VDPQuery Identity))

data Timeout = Timeout deriving (Show,Eq,Typeable)

data Pair a = Pair a a deriving (Show, Eq, Functor) 

