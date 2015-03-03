{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
    ) where

import VDPQ.Types

import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Map
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
buildVDPURLPair query = ((url, opts), (url', opts'))
  where
    url = mconcat [
          "http://", _vdpHost server, ":", show (_vdpPort server)
        , "/denodo-restfulws/", _vdpDatabase server
        , "/views/", _viewName query
        ]

    url' = mconcat [ url, "/$schema" ]

    opts = 
        (set (param "$format") ["JSON"] .
         set auth (Just auth_))
        defaults 

    opts' =
        (set (param "$filter") filters .
         set (param "$displayRESTfulReferences") ["false"])
        opts

    server = (runIdentity . _targetVDP) query

    auth_ = basicAuth 
        (fromString (_vdpLogin server))
        (fromString (_vdpPassword server))

    filters = toListOf (whereClause . folded . to fromString) query
