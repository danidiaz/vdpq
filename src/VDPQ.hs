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
    ,   buildVDPSchemaURL 
    ,   buildVDPURL 
    ) where

import VDPQ.Types

import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Map
import Control.Lens

import Formatting

import Network.Wreq

defaultVDPServer :: VDPServer
defaultVDPServer = VDPServer "localhost" 9090 "admin" "admin" "admin"

defaultTemplateName :: String
defaultTemplateName = "_template"

examplePlan :: Plan Maybe
examplePlan = Plan
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
         -> Plan Maybe -> Plan Identity
fillPlan f plan = over vdp f plan

defaultFillPlan :: Plan Maybe -> Plan Identity
defaultFillPlan = fillPlan defaultFillVDPTargets

queryList :: Plan Identity -> [Query]
queryList (Plan vdp) = undefined 
     
buildVDPBaseURL :: VDPQuery Identity -> (T.Text,Options)  
buildVDPBaseURL q = (url,opts) 
  where
    server = (runIdentity . _targetVDP) q
    auth' = basicAuth 
        (fromString (_vdpLogin server))
        (fromString (_vdpPassword server))

    url = sformat 
        ("http://" % string % ":" % int %
            "/denodo-restfulws/" % string % "/views/" % string ) 
        (_vdpHost server)
        (_vdpPort server)
        (_vdpDatabase server)
        (_viewName q)
    opts = set (param "$format") ["JSON"]
         . set auth (Just auth')
         $ defaults 

buildVDPSchemaURL :: VDPQuery Identity -> (T.Text,Options)
buildVDPSchemaURL q = 
    over _1 (sformat (stext % "/$schema")) (buildVDPBaseURL q)

buildVDPURL :: VDPQuery Identity -> (T.Text,Options)
buildVDPURL q = 
      set (_2.param "$filter") (toListOf filterl q)  
    . set (_2.param "$displayRESTfulReferences") ["false"] 
    $ buildVDPBaseURL q
  where
    filterl = whereClause . folded . to (sformat string)
