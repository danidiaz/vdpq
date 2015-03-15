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

newtype JSONResponse = JSONResponse { getJSONReponse :: Value  } deriving (Show,Eq,Typeable,Generic)

instance FromJSON JSONResponse where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON JSONResponse where
    toJSON = genericToJSON aesonOptions

newtype ResponseError = ResponseError String deriving (Show,Eq,Typeable)

instance IsString ResponseError where
    fromString = ResponseError


newtype URL = URL { getURL :: String } deriving Generic

instance FromJSON URL where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON URL where
    toJSON = genericToJSON aesonOptions

data Schema a b = Schema
    {
        _vdp :: a
    ,   _json :: b
    } 
    deriving (Generic, Show)

$(makeLenses ''Schema)


-- Boilerplate time !!!!!
uniformSchema :: a -> Schema a a
uniformSchema a = Schema a a

traverseSchema :: (Applicative f) 
               => Schema (a -> f a')
                         (b -> f b')
               -> Schema a b 
               -> f (Schema a' b')
traverseSchema (Schema fa fb) (Schema ta tb) = Schema <$> fa ta <*> fb tb

apSchema :: Schema (a -> a')
                   (b -> b')
         -> Schema a b
         -> Schema a' b'
apSchema (Schema fa fb) (Schema a b) = Schema (fa a) (fb b)

foldMapSchema :: (Monoid m) 
              => Schema (a -> m)
                        (b -> m)
              -> Schema a b
              -> m
foldMapSchema (Schema fa fb) (Schema a b) = fa a <> fb b

namesSchema :: Schema String String
namesSchema = Schema "vdp" "json"
-- boilerplate end.

type Plan_ = Schema (Map String (VDPQuery Maybe))
                    (Map String URL)

instance FromJSON Plan_ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan_ where
    toJSON = genericToJSON aesonOptions

type Plan = Schema (Map String (VDPQuery Identity))
                   (Map String URL)

data Timeout = Timeout deriving (Show,Eq,Typeable)

data Pair a = Pair a a deriving (Show, Eq, Functor) 

