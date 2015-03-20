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

data Schema a b c d e = Schema
    {
        _vdp :: a
    ,   _json :: b
    ,   _xml :: c
    ,   _rss :: d
    ,   _html :: e
    } 
    deriving (Generic, Show)

$(makeLenses ''Schema)


-- Boilerplate time !!!!!
-- I *really* should use something like Vinyl for this...
uniformSchema :: a -> Schema a a a a a
uniformSchema a = Schema a a a a a

traverseSchema :: (Applicative f) 
               => Schema (a -> f a')
                         (b -> f b')
                         (c -> f c')
                         (d -> f d')
                         (e -> f e')
               -> Schema a b c d e
               -> f (Schema a' b' c' d' e')
traverseSchema (Schema fa fb fc fd fe) (Schema ta tb tc td te) = Schema <$> fa ta <*> fb tb <*> fc tc <*> fd td <*> fe te

apSchema :: Schema (a -> a')
                   (b -> b')
                   (c -> c')
                   (d -> d')
                   (e -> e')
         -> Schema a b c d e
         -> Schema a' b' c' d' e'
apSchema (Schema fa fb fc fd fe) (Schema a b c d e) = Schema (fa a) (fb b) (fc c) (fd d) (fe e)

foldMapSchema :: (Monoid m) 
              => Schema (a -> m)
                        (b -> m)
                        (c -> m)
                        (d -> m)
                        (e -> m)
              -> Schema a b c d e
              -> m
foldMapSchema (Schema fa fb fc fd fe) (Schema a b c d e) = fa a <> fb b <> fc c <> fd d <> fe e

namesSchema :: Schema String String String String String
namesSchema = Schema "vdp" "json" "xml" "rss" "html"
-- boilerplate end.

type Plan_ = Schema (Map String (VDPQuery Maybe))
                    (Map String URL)
                    (Map String URL)
                    (Map String URL)
                    (Map String URL)

instance FromJSON Plan_ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Plan_ where
    toJSON = genericToJSON aesonOptions

type Plan = Schema (Map String (VDPQuery Identity))
                   (Map String URL)
                   (Map String URL)
                   (Map String URL)
                   (Map String URL)

data Timeout = Timeout deriving (Show,Eq,Typeable)

data Pair a = Pair a a deriving (Show, Eq, Functor) 

