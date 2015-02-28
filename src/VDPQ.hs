{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
import Data.Map
import Control.Lens

import Formatting

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
     

buildVDPBaseURL :: VDPQuery Identity -> T.Text  
buildVDPBaseURL q = sformat 
    ("http://" % string % ":" % int %
        "/denodo-restfulws/" % string % "/views/" % string ) 
    (_vdpHost server)
    (_vdpPort server)
    (_vdpDatabase server)
    (_viewName q)
  where
    server = (runIdentity . _targetVDP) q

buildVDPSchemaURL :: VDPQuery Identity -> (T.Text,[(T.Text, T.Text)])
buildVDPSchemaURL q = (url,params)
  where
    url = sformat (stext % "/$schema") (buildVDPBaseURL q)
    params = ("$format","JSON") : []

buildVDPURL :: VDPQuery Identity -> (T.Text,[(T.Text, T.Text)])
buildVDPURL q = (buildVDPBaseURL q, params)
  where
    params = ("$format","JSON") : toListOf filterL q
    filterL = whereClause 
            . folded 
            . to ((,) "$filter" . sformat string)


