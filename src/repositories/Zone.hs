{-# LANGUAGE OverloadedStrings #-}

module Zone where

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
import ZoneModel (ZoneModel(..))
import SQLModel
import Control.Exception (try)




findZoneAll :: Connection -> IO (Either DBError [ZoneModel])
findZoneAll conn = do
    result <- try $ query_ conn "SELECT state, name, zone_id  FROM zones"
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right msgs -> return $ Right msgs


insertZone :: Connection -> ZoneModel -> IO (Either DBError UUID)
insertZone conn zone = do
    uuid <- nextRandom
    let query = "INSERT INTO zones (state, name, zone_id) VALUES (?,?,?)"
    res <- try $ execute conn query
        ( zone.zoneState
        , zone.zoneName
        , uuid
        ) :: IO (Either SQLError ())
    case res of
        Left err -> do
            putStrLn $ show err
            return $ Left $ DatabaseError (SQLError ErrorError "1" "")
        Right _ ->  return $ Right uuid