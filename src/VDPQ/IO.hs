{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAsync
    ,   loadJSON
    ,   loadPlan
    ,   Seconds(..)
    ,   runVDPQuery 
    ,   withTimeLimit
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
safeGET :: (String,Options) ->  ExceptT String IO Value
safeGET (url,opts) =  do
    r <- tryAsync (getWith opts url) 
    let status = view (responseStatus.statusCode) r
    if status == 204
        then return Null
        else do
            unless (status == 200) . throwE $
                ("Received HTTP status code: " <> show status)
            rjson <- tryAsync (asValue r) -- throws JSON error
            return . view responseBody $ rjson

newtype Seconds = Seconds Int

toMicros :: Seconds -> Int
toMicros (Seconds s) = s * 10^(6::Int)

withTimeLimit :: Seconds -> IO a -> IO (Either () a)
withTimeLimit (toMicros -> micros) =  race (threadDelay micros)

runVDPQuery :: VDPQuery Identity -> IO (Either String (Value,Value))
runVDPQuery query = 
    let (schemaurl,dataurl) = buildVDPURLPair query
    in runExceptT (liftA2 (,) (safeGET schemaurl) (safeGET dataurl))


