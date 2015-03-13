{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module VDPQ 
    (
        module VDPQ.Types
    ,   defaultVDPServer
    ,   defaultTemplateName
    ,   examplePlan
    ,   vdpQueryDefault
    ,   fillVDPTargets
    ,   defaultFillVDPTargets 
    ,   fillPlan
    ,   defaultFillPlan
    ,   buildVDPURLPair 
    ,   Reportable(..)
    ,   responseReport 
    ) where

import VDPQ.Types

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Bifoldable
import qualified Data.Text as T
import Data.Map
import Control.Applicative
import Control.Lens

import Network.Wreq


defaultVDPServer :: VDPServer
defaultVDPServer = VDPServer "localhost" 9090 "admin" "admin" "admin"

defaultTemplateName :: String
defaultTemplateName = "_template"

examplePlan :: Plan_ 
examplePlan = Schema
    (Data.Map.fromList
        [ (defaultTemplateName, VDPQuery "fooview" (Just "where 1 = 1") (Just defaultVDPServer))
        , ("q1", VDPQuery "fooview" (Just "where 1 = 1") Nothing)
        , ("q2", VDPQuery "barview" Nothing Nothing) 
        ]
    )

vdpQueryDefault :: VDPServer -> VDPQuery Maybe -> VDPQuery Identity 
vdpQueryDefault dt = over targetVDP (Identity . maybe dt id)

fillVDPTargets :: VDPServer 
               -> String
               -> Map String (VDPQuery Maybe) 
               -> Map String (VDPQuery Identity) 
fillVDPTargets fallbackServer templateName qs =
    let template = preview (ix templateName . targetVDP . folded) qs 
        server = fromMaybe fallbackServer template
        qs' = filterWithKey (\k _ -> head k /= '_') qs
    in  fmap (vdpQueryDefault server) qs'

defaultFillVDPTargets :: Map String (VDPQuery Maybe) 
                      -> Map String (VDPQuery Identity) 
defaultFillVDPTargets =  fillVDPTargets defaultVDPServer defaultTemplateName

fillPlan :: (Map String (VDPQuery Maybe) -> Map String (VDPQuery Identity))
         -> Plan_ -> Plan
fillPlan f plan = over vdp f plan

defaultFillPlan :: Plan_ -> Plan
defaultFillPlan = fillPlan defaultFillVDPTargets

buildVDPURLPair :: VDPQuery Identity -> ((String, Options), (String, Options))
buildVDPURLPair query = ((schemaurl, schemaopts), (dataurl, dataopts))
  where
    dataurl = mconcat [
          "http://", _vdpHost server, ":", show (_vdpPort server)
        , "/denodo-restfulws/", _vdpDatabase server
        , "/views/", _viewName query
        ]

    schemaurl = mconcat [ dataurl, "/$schema" ]

    schemaopts = 
        (set (param "$format") ["JSON"] .
         set auth (Just auth_))
        defaults 

    dataopts =
        (set (param "$filter") filters .
         set (param "$displayRESTfulReferences") ["false"])
        schemaopts

    server = (runIdentity . _targetVDP) query

    auth_ = basicAuth 
        (fromString (_vdpLogin server))
        (fromString (_vdpPassword server))

    filters = toListOf (whereClause . folded . to fromString) query


class Reportable a where
    getReport :: a -> [String]

instance (Bifoldable f, Reportable a, Reportable b) => Reportable (f a b) where
    getReport = bifoldMap getReport getReport 

instance Reportable VDPResponse where
    getReport (VDPResponse _ data') =
        case data' of
            Null -> ("Empty result.") : []
            _ -> []

instance Reportable Timeout where 
    getReport _ = ("Timeout.") : []

instance Reportable ResponseError where 
    getReport (ResponseError errmsg) = ["Error: " ++ errmsg]


responseReport :: (FoldableWithIndex String f, Reportable a) 
               => Schema (f a)
               -> [((String,String),[String])]
responseReport response = 
    let foldFunc = \name -> ifoldMap $ \test ->
           pure . (,) (name,test) . getReport 
        reportSchema = Schema
            foldFunc        
    in reportSchema `apSchema` namesSchema `foldMapSchema` response   




