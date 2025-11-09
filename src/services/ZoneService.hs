{-# LANGUAGE OverloadedStrings #-}

module ZoneService(createZone, getZoneAll) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID, toString)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Lens.Internal.TH (prismTypeName)

import Database.SQLite.Simple (Connection)

import GHC.Int

import ErrorMessage
import Views

import Zone
import ZoneModel
import SQLModel


getZoneAll conn = do
    liftIO $ print "Fetching all Zone"
    result <- liftIO $ findZoneAll conn
    case result of
        Left err -> do
                jsonResponse (ErrorMessage "Zone not found")
                status notFound404
        Right zone -> do
                status ok200
                jsonResponse zone


createZone conn =  do
    bodyContent <- body
    let zoneRequest = decode bodyContent :: Maybe ZoneModel
    case zoneRequest of
        Just zone -> do
            result <- liftIO $ insertZone conn zone
            case result of
                Left (DatabaseError err) -> do
                    status status500
                    jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
                Right uuid -> do
                    jsonResponse $ SuccessMessage $ pack $ toString uuid
                    status status201
              
        Nothing -> do
            -- If the body cannot be parsed as ZoneRequest, return an error
            jsonResponse (ErrorMessage "Invalid Zone request format")
            status badRequest400