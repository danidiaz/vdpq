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
import Text.XML

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

newtype XMLResponse = XMLResponse { getXMLReponse :: Document  } deriving (Show,Eq,Typeable)

newtype ResponseError = ResponseError String deriving (Show,Eq,Typeable)

instance IsString ResponseError where
    fromString = ResponseError


newtype URL = URL { getURL :: String } deriving Generic

instance FromJSON URL where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON URL where
    toJSON = genericToJSON aesonOptions

data Schema a b c = Schema
    {
        _vdp :: a
    ,   _json :: b
    ,   _xml :: c
    } 
    deriving (Generic, Show)

$(makeLenses ''Schema)


-- Boilerplate time !!!!!
-- I *really* should use something like Vinyl for this...
uniformSchema :: a -> Schema a a a
uniformSchema a = Schema a a a

traverseSchema :: (Applicative f) 
               => Schema (a -> f a')
                         (b -> f b')
                         (c -> f c')
               -> Schema a b c
               -> f (Schema a' b' c')
traverseSchema (Schema fa fb fc) (Schema ta tb tc) = Schema <$> fa ta <*> fb tb <*> fc tc

apSchema :: Schema (a -> a')
                   (b -> b')
                   (c -> c')
         -> Schema a b c
         -> Schema a' b' c'
apSchema (Schema fa fb fc) (Schema a b c) = Schema (fa a) (fb b) (fc c)

foldMapSchema :: (Monoid m) 
              => Schema (a -> m)
                        (b -> m)
                        (c -> m)
              -> Schema a b c
              -> m
foldMapSchema (Schema fa fb fc) (Schema a b c) = fa a <> fb b <> fc c

namesSchema :: Schema String String String
namesSchema = Schema "vdp" "json" "xml"
-- boilerplate end.

type Plan_ = Schema (Map String (VDPQuery Maybe))
                    (Map String URL)
                    (Map String URL)

instance FromJSON Plan_ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan_ where
    toJSON = genericToJSON aesonOptions

type Plan = Schema (Map String (VDPQuery Identity))
                   (Map String URL)
                   (Map String URL)

data Timeout = Timeout deriving (Show,Eq,Typeable)

data Pair a = Pair a a deriving (Show, Eq, Functor) 

