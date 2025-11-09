{-# LANGUAGE OverloadedStrings #-}

module Relation where

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
import RelationModel (RelationModel(..))
import SQLModel
import Control.Exception (try)



findRelationAll :: Connection -> IO (Either DBError [RelationModel])
findRelationAll conn = do
    result <- try $ query_ conn "SELECT zone_id, sensor_id  FROM relations"
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right msgs -> return $ Right msgs



insertRelation :: Connection -> RelationModel -> IO (Either DBError UUID)
insertRelation conn relation = do
    let query = "INSERT INTO relations (zone_id, sensor_id) VALUES (?,?)"
    res <- try $ execute conn query
        ( relation.relationZoneId 
        , relation.relationSensorId
        ) :: IO (Either SQLError ())
    case res of
        Left err -> do
            putStrLn $ show err
            return $ Left $ DatabaseError (SQLError ErrorError "1" "")
        Right _ ->  return $ Right relation.relationZoneId
