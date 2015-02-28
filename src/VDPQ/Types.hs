{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module VDPQ.Types where

import Data.Map (Map)
import Data.Aeson
import Data.Aeson.Types

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

instance FromJSON (VDPQuery Maybe) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON (VDPQuery Maybe) where
    toJSON = genericToJSON aesonOptions

$(makeLenses ''VDPQuery)


data Plan f = Plan 
    {
        _vdp :: Map String (VDPQuery f)
    } 
    deriving (Generic)

instance FromJSON (Plan Maybe) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON (Plan Maybe) where
    toJSON = genericToJSON aesonOptions


$(makeLenses ''Plan)


data Query = VDP (VDPQuery Identity) | HTTP | Stomp deriving (Generic, Show)
