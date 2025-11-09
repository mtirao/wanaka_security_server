{-# LANGUAGE OverloadedStrings #-}

module SensorService(createSensor, getSensorAll) where

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

import Sensor
import SensorModel
import SQLModel


getSensorAll conn = do
    liftIO $ print "Fetching all Sensors"
    result <- liftIO $ findSensorAll conn
    case result of
        Left err -> do
                jsonResponse (ErrorMessage "Sensors not found")
                status notFound404
        Right sensor -> do
                status ok200
                jsonResponse sensor

createSensor conn =  do
    bodyContent <- body
    let sensorRequest = decode bodyContent :: Maybe SensorModel
    case sensorRequest of
        Just sensor -> do
            result <- liftIO $ insertSensor conn sensor
            case result of
                Left (DatabaseError err) -> do
                    status status500
                    jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
                Right uuid -> do
                    jsonResponse $ SuccessMessage $ pack $ toString uuid
                    status status201
              
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid Sensor request format")
            status badRequest400