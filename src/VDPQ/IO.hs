{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAsync
    ,   loadJSON
    ,   loadPlan
    ,   runVDPQuery 
    ) where

import VDPQ

import BasePrelude hiding ((%))
import MTLPrelude

import Data.Map
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad
import Control.Monad.Trans.Except
import Control.Lens
import Control.Concurrent.Async

import System.Directory
import Formatting
import Network.Wreq


tryAsync :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAsync action = withExceptT show $
    ExceptT (liftIO (withAsync action waitCatch))


loadJSON :: FromJSON a => FilePath -> ExceptT String IO a
loadJSON path = 
    withExceptT ("Loading JSON: "++) $ do
        bytes <- tryAsync (B.readFile path)
        ExceptT (return (eitherDecode (BL.fromStrict bytes)))


loadPlan :: FilePath -> ExceptT String IO (Plan Maybe)
loadPlan  = loadJSON 

-- Things that can go wrong:
--      connection error
--      non-200, non-204 return codes
--      decoding error
safeGET :: (T.Text,Options) ->  ExceptT String IO Value
safeGET (T.unpack -> url,opts) =  do
    r <- tryAsync (getWith opts url) 
    let status = view (responseStatus.statusCode) r
    if status == 204
        then return Null
        else do
            unless (status == 200) . throwE $
                T.unpack (sformat ("Received status code: " % int) status)
            rjson <- tryAsync (asValue r) -- throws JSON error
            return . view responseBody $ rjson

runVDPQuery :: VDPQuery Identity -> ExceptT String IO (Value,Value)
runVDPQuery q = 
    liftA2 (,) (safeGET (buildVDPSchemaURL q)) (safeGET (buildVDPURL q))


