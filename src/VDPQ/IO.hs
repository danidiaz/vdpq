{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAnyS
    ,   hWriteJSON
    ,   loadJSON
    ,   loadPlan
    ,   Seconds(..)
    ,   withTimeout
    ,   withLog
    ,   withConc
    ,   runVDPQuery 
    ,   executorSchema
    ,   FromFolder(..)
    ,   ToFolder(..)
    ,   writeReport
    ) where

import VDPQ

import Data.Bifoldable
import Data.List
import Data.Monoid
import Data.Map
import Data.String
import Data.Proxy
import qualified Data.Set as S
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Lens
import Control.Concurrent.Async

import Control.Exception
import Control.Exception.Enclosed

import qualified Network.Wreq as W

import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F

import System.IO

tryAnyS :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAnyS = withExceptT show . ExceptT . liftIO . tryAny

loadJSON :: FromJSON a => F.FilePath -> IO a
loadJSON path = do 
    bytes <- F.readFile path
    let e = eitherDecodeStrict' bytes
    case e of 
        Left err -> ioError (userError err)
        Right p -> return p

hWriteJSON :: ToJSON a => Handle -> a -> IO () 
hWriteJSON h =  BL.hPutStr h . encodePretty 

writeJSON :: ToJSON a => a -> F.FilePath -> IO () 
writeJSON a path  = F.withFile path F.WriteMode $ \h -> hWriteJSON h a

loadPlan :: F.FilePath -> IO Plan_
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
    bracket_ (go '+' (const id) S.insert) (go '-' S.delete (const id)) (f i a)
  where
    name = prefix <> "/" <> i
    go c enter exit = modifyMVar_ pending $ \names -> do
        let names' = enter name names
        hPutStr stderr ([c] <> name)
        hPutStrLn stderr (" " <> intercalate "," (F.toList names'))
        hFlush stderr 
        return (exit name names')

withConc :: QSem -> (i -> a -> IO b) -> (i -> a -> Concurrently b)
withConc sem f = \i a -> Concurrently 
    (bracket_ (waitQSem sem) (signalQSem sem) (f i a))

type Executor = Schema (String -> 
                        VDPQuery Identity -> 
                        IO (Either ResponseError VDPResponse))

executorSchema :: Executor
executorSchema = Schema
    (\_ -> runVDPQuery)


class ToFolder t where
    writeToFolder :: F.FilePath -> t -> IO ()

class FromFolder t where
    readFromFolder :: F.FilePath -> IO t

class FromFolder t => InFolder t where
    existsInFolder :: F.FilePath -> Proxy t -> IO Bool


instance (ToFolder a, ToFolder b) => ToFolder (Either a b) where
    writeToFolder folder =
        bitraverse_ (writeToFolder folder) (writeToFolder folder)

instance (InFolder a,FromFolder b) => FromFolder (Either a b) where
    readFromFolder path = do
        exists <- existsInFolder path (Proxy::Proxy a)
        if exists 
            then Left <$> readFromFolder path
            else Right <$> readFromFolder path

timeoutFileName :: F.FilePath
timeoutFileName = "_timeout_"

instance ToFolder Timeout where
    writeToFolder path Timeout  = 
        F.writeTextFile (path <> timeoutFileName) mempty

instance FromFolder Timeout where
    readFromFolder _ = return Timeout

instance InFolder Timeout where
    existsInFolder path _ = F.isFile (path <> timeoutFileName)

errorFileName :: F.FilePath
errorFileName  = "_error_.txt"

instance ToFolder ResponseError where
    writeToFolder path (ResponseError msg) = 
        F.writeTextFile (path <> errorFileName) (fromString msg)

instance FromFolder ResponseError where
    readFromFolder path =
        ResponseError . T.unpack <$> F.readTextFile (path <> errorFileName)

instance InFolder ResponseError where
    existsInFolder path _ = F.isFile (path <> errorFileName)


vdpResponseFileNames :: (F.FilePath,F.FilePath)
vdpResponseFileNames = ("vdp.schema.json","vdp.data.json")

instance ToFolder VDPResponse where
    writeToFolder path (VDPResponse s d) =  
        let (sf,df) = vdpResponseFileNames 
        in writeJSON s (path <> sf) >> writeJSON d (path <> df) 

instance FromFolder VDPResponse where
    readFromFolder path = let (sf,df) = vdpResponseFileNames in
         VDPResponse <$> loadJSON (path <> sf) <*> loadJSON (path <> df)    

instance (ToFolder a) => ToFolder (Map String a) where
    writeToFolder path m =  
        let writerfunc = \i x -> do
                let path' = path <> fromString i
                F.createDirectory False path'
                writeToFolder path' x
        in itraverse_ writerfunc m  

instance (FromFolder a) => FromFolder (Map String a) where
    readFromFolder path = do
        allContents <- F.listDirectory path
        folders <- filterM F.isDirectory allContents
        let mkPair f = (,) (F.encodeString (F.filename f)) <$> readFromFolder f
        pairs <- T.mapM mkPair folders
        return (Data.Map.fromList pairs)


instance (ToFolder a) => ToFolder (Schema a) where
    writeToFolder path s = do
       let calcPath = (<>) path . fromString 
           pathSchema = uniformSchema calcPath `apSchema` namesSchema
       let writeFunc = F.createDirectory False
       _ <- traverseSchema (uniformSchema writeFunc) pathSchema 
       let writeSchema = Schema
              writeToFolder 
       _ <- traverseSchema (writeSchema `apSchema` pathSchema) s
       return ()
            

instance (FromFolder a) => FromFolder (Schema a) where
    readFromFolder path = do
        let readFunc name = readFromFolder (path <> fromString name)  
            readerSchema = Schema
                readFunc
        traverseSchema readerSchema namesSchema
        

writeReport :: [((String,String),[String])] -> IO ()
writeReport entries =   
    F.forM_ entries $ \((tag,test), report) -> do
        putStrLn (tag ++ "/" ++ test)
        F.forM_ report $ \str ->
            putStrLn ("\t" ++ take 80 str)

