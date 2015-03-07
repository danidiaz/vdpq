{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAnyS
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

import Data.Bifoldable
import Data.List
import Data.Monoid
import Data.Map
import Data.Proxy
import qualified Data.Set as S
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Foldable as F
import qualified Data.Text as T
--import qualified Data.Text.Lazy.IO as TLIO
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad
import Control.Monad.Trans.Except
import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Concurrent.MVar

import System.Directory
import System.IO
import qualified Network.Wreq as W

import Control.Exception.Enclosed

import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F

tryAnyS :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAnyS = withExceptT show . ExceptT . liftIO . tryAny

loadJSON :: FromJSON a => F.FilePath -> ExceptT String IO a
loadJSON path = 
    withExceptT ("Loading JSON: "++) $ do
        bytes <- tryAnyS (F.readFile path)
        ExceptT (return (eitherDecodeStrict' bytes))

loadJSON' :: FromJSON a => F.FilePath -> IO a
loadJSON' path = runExceptT (loadJSON path) >>= return . either error id 

writeJSON :: ToJSON a => a -> F.FilePath -> IO () 
writeJSON a path  = F.withFile path F.WriteMode $ \h ->
    (BL.hPutStr h . encodePretty) a

loadPlan :: F.FilePath -> ExceptT String IO Plan_
loadPlan  = loadJSON 

-- Things that can go wrong:
--      connection error
--      non-200, non-204 return codes
--      decoding error
safeGET :: (String, W.Options) ->  ExceptT String IO Value
safeGET (url,opts) =  do
    r <- tryAnyS (W.getWith opts url) 
    let status = view (W.responseStatus.W.statusCode) r
    if status == 204
        then return Null
        else do
            (unless (status == 200) . throwE) 
                ("Received HTTP status code: " <> show status)
            rjson <- tryAnyS (W.asValue r) -- throws JSON error
            return . view W.responseBody $ rjson


runVDPQuery :: VDPQuery Identity -> IO (Either ResponseError VDPResponse)
runVDPQuery query = 
    let (schemaurl,dataurl) = buildVDPURLPair query
    in (runExceptT . withExceptT ResponseError)
       (liftA2 VDPResponse (safeGET schemaurl) (safeGET dataurl))

newtype Seconds = Seconds Int

toMicros :: Seconds -> Int
toMicros (Seconds s) = s * 10^(6::Int)

data Timeout = Timeout deriving (Show)

withTimeout :: Seconds 
            -> (i -> a -> IO b) 
            -> (i -> a -> IO (Either Timeout b))
withTimeout (toMicros -> micros) f = \i a -> 
    race (threadDelay micros *> pure Timeout) (f i a) 

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


basicExecutor :: Schema (String -> 
                         VDPQuery Identity -> 
                         IO (Either ResponseError VDPResponse))
basicExecutor = Schema
    (\_ -> runVDPQuery)


class ToFolder t where
    writeToFolder :: F.FilePath -> t -> IO ()

class FromFolder t where
    readFromFolder :: F.FilePath -> IO t
    existsInFolder :: F.FilePath -> Proxy t -> IO Bool

instance (ToFolder a, ToFolder b) => ToFolder (Either a b) where
    writeToFolder folder =
        bitraverse_ (writeToFolder folder) (writeToFolder folder)

instance (FromFolder a,FromFolder b) => FromFolder (Either a b) where
    readFromFolder path = do
        exists <- existsInFolder path (Proxy::Proxy a)
        if exists 
            then Left <$> readFromFolder path
            else Right <$> readFromFolder path

    existsInFolder path _ = 
        (||) <$> existsInFolder path (Proxy::Proxy a) 
             <*> existsInFolder path (Proxy::Proxy b) 

timeoutFileName :: F.FilePath
timeoutFileName = "_timeout_"

instance ToFolder Timeout where
    writeToFolder path Timeout  = 
        F.writeTextFile (path <> timeoutFileName) mempty

instance FromFolder Timeout where
    readFromFolder _ = return Timeout
    existsInFolder path _ = F.isFile (path <> timeoutFileName)

errorFileName :: F.FilePath
errorFileName  = "_error_.txt"

instance ToFolder ResponseError where
    writeToFolder path (ResponseError msg) = 
        F.writeTextFile (path <> errorFileName) (fromString msg)

instance FromFolder ResponseError where
    readFromFolder path =
        ResponseError . T.unpack <$> F.readTextFile (path <> errorFileName)
    existsInFolder path _ = F.isFile (path <> errorFileName)


vdpResponseFileNames :: (F.FilePath,F.FilePath)
vdpResponseFileNames = ("schema.json","data.json")

instance ToFolder VDPResponse where
    writeToFolder path (VDPResponse s d) =  
        let (sf,df) = vdpResponseFileNames 
        in writeJSON s (path <> sf) >> writeJSON d (path <> df) 

instance FromFolder VDPResponse where
    readFromFolder path = let (sf,df) = vdpResponseFileNames in
         VDPResponse <$> loadJSON' (path <> sf) <*> loadJSON' (path <> df)    
    existsInFolder path _ = 
        let (sf,_) = vdpResponseFileNames in F.isFile (path <> sf) 



