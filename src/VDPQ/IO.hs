{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAny'
    ,   loadJSON
    ,   loadPlan
    ,   Seconds(..)
    ,   withTimeout
    ,   withLog
    ,   withConc
    ,   runVDPQuery 
    ,   basicExecutor
    ) where

import VDPQ

import BasePrelude hiding ((%))
import MTLPrelude

import Data.List
import Data.Map
import qualified Data.Set as S
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad
import Control.Monad.Trans.Except
import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Concurrent.MVar

import System.Directory
import System.IO
import Network.Wreq

import Control.Exception.Enclosed

tryAny' :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAny' = withExceptT show . ExceptT . liftIO . tryAny

loadJSON :: FromJSON a => FilePath -> ExceptT String IO a
loadJSON path = 
    withExceptT ("Loading JSON: "++) $ do
        bytes <- tryAny' (B.readFile path)
        ExceptT (return (eitherDecode (BL.fromStrict bytes)))


loadPlan :: FilePath -> ExceptT String IO Plan_
loadPlan  = loadJSON 

-- Things that can go wrong:
--      connection error
--      non-200, non-204 return codes
--      decoding error
safeGET :: (String,Options) ->  ExceptT String IO Value
safeGET (url,opts) =  do
    r <- tryAny' (getWith opts url) 
    let status = view (responseStatus.statusCode) r
    if status == 204
        then return Null
        else do
            (unless (status == 200) . throwE) 
                ("Received HTTP status code: " <> show status)
            rjson <- tryAny' (asValue r) -- throws JSON error
            return . view responseBody $ rjson

newtype Seconds = Seconds Int

toMicros :: Seconds -> Int
toMicros (Seconds s) = s * 10^(6::Int)

withTimeout :: Seconds 
            -> (i -> a -> IO b) 
            -> (i -> a -> IO (Either () b))
withTimeout (toMicros -> micros) f = \i a -> 
    race (threadDelay micros) (f i a) 

withLog :: MVar (S.Set String) 
        -> String
        -> (String -> a -> IO b)
        -> (String -> a -> IO b)
withLog pending prefix f i a =
    bracket_ (op '+' (const id) S.insert) (op '-' S.delete (const id)) (f i a)
  where
    name = prefix <> "/" <> i
    op c enter exit = modifyMVar_ pending $ \names -> do
        let names' = enter name names
        hPutStr stderr ([c] <> name)
        hPutStrLn stderr (" " <> intercalate "," (F.toList names'))
        hFlush stderr 
        return (exit name names')

withConc :: QSem -> (i -> a -> IO b) -> (i -> a -> Concurrently b)
withConc sem f = \i a -> Concurrently 
    (bracket_ (waitQSem sem) (signalQSem sem) (f i a))

runVDPQuery :: VDPQuery Identity -> IO (Either String (Value,Value))
runVDPQuery query = 
    let (schemaurl,dataurl) = buildVDPURLPair query
    in runExceptT (liftA2 (,) (safeGET schemaurl) (safeGET dataurl))

basicExecutor :: Schema (String -> 
                         VDPQuery Identity -> 
                         IO (Either String (Value,Value)))
basicExecutor = Schema
    (\_ -> runVDPQuery)



