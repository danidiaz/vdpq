module VDPQ 
    (
        module VDPQ.Types
    ,   defaultVDPServer
    ,   defaultTemplateName
    ,   examplePlan
    ,   vdpQueryDefault
    ,   fillVDPTargets
    ,   defaultFillVDPTargets 
    ) where

import VDPQ.Types

import Data.Maybe
import Data.Map
import Control.Lens

defaultVDPServer :: VDPServer
defaultVDPServer = VDPServer "localhost" 9999 "admin" "admin" "admin"

defaultTemplateName :: String
defaultTemplateName = "_template"

examplePlan :: Plan
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
fillVDPTargets fallback templateName qs =
    let template = preview (ix templateName.targetVDP.folded) qs 
        server' = fromMaybe fallback template
        qs' = filterWithKey (\k _ -> head k == '_') qs
    in  fmap (vdpQueryDefault server') qs'

defaultFillVDPTargets :: Map String (VDPQuery Maybe) 
                      -> Map String (VDPQuery Identity) 
defaultFillVDPTargets =  fillVDPTargets defaultVDPServer defaultTemplateName




