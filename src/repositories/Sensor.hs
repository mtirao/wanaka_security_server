{-# LANGUAGE OverloadedStrings #-}

module Sensor where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import Data.UUID (UUID, fromText, toText, toString, fromString)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import GHC.Int (Int64)
import SensorModel (SensorModel(..))
import SQLModel
import Control.Exception (try)



findSensorAll :: Connection -> IO (Either DBError [SensorModel])
findSensorAll conn = do
    result <- try $ query_ conn "SELECT status, type, location, sensor_id  FROM sensors"
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right msgs -> return $ Right msgs


findSensorByZoneId :: Connection -> UUID -> IO (Either DBError [SensorModel])
findSensorByZoneId conn zoneId = do
    let q = "SELECT s.status, s.type, s.location, s.sensor_id \
                \FROM relations \
                \NATURAL JOIN zones \
                \NATURAL JOIN sensors s \
                \WHERE zone_id LIKE ?"
    result <- try $ query conn q (Only zoneId)
    case result of
        Left err -> do 
            putStrLn $ show err
            return $ Left $ DatabaseError err
        Right sensors -> return $ Right sensors

insertSensor :: Connection -> SensorModel -> IO (Either DBError UUID)
insertSensor conn sensor = do
    uuid <- nextRandom
    let query = "INSERT INTO sensors (status, type, location, sensor_id) VALUES (?,?,?,?)"
    res <- try $ execute conn query
        ( sensor.sensorStatus
        , sensor.sensorType
        , sensor.sensorLocation
        , uuid
        ) :: IO (Either SQLError ())
    case res of
        Left err -> do
            putStrLn $ show err
            return $ Left $ DatabaseError (SQLError ErrorError "1" "")
        Right _ ->  return $ Right uuid
