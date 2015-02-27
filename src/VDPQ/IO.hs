{-# LANGUAGE NoImplicitPrelude #-}

module VDPQ.IO 
    (
        module VDPQ
    ,   tryAsync
    ,   loadJSON
    ,   loadPlan
    ) where

import VDPQ

import BasePrelude
import MTLPrelude

import Data.Map
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Control.Concurrent.Async

import System.Directory


tryAsync :: (Functor m, MonadIO m) => IO a -> ExceptT String m a
tryAsync action = withExceptT show (ExceptT (liftIO (withAsync action waitCatch)))


loadJSON :: FromJSON a => FilePath -> ExceptT String IO a
loadJSON path = 
    withExceptT ("Loading JSON: "++) $ do
        bytes <- tryAsync (B.readFile path)
        ExceptT (return (eitherDecode (BL.fromStrict bytes)))

loadPlan :: FilePath -> ExceptT String IO Plan
loadPlan  = fmap (id::Plan -> Plan) . loadJSON 
