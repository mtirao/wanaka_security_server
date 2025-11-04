{-# LANGUAGE OverloadedStrings #-}

module Activity where

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
import ActivityModel (ActivityModel(..), ActivityContent(..))
import SQLModel
import Control.Exception (try)


findActivity :: Connection -> UUID -> IO (Either DBError ActivityModel)
findActivity conn id = do
    result <- try $ query conn "SELECT * FROM activities WHERE id = ?" (Only id)
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right [] -> return $ Left $ ActivityNotFound id
        Right [activity] -> return $ Right activity
        Right _ -> return $ Left $ DatabaseError (SQLError ErrorError "2" "Multiple activities found for single ID")


findActivityAll :: Connection -> IO (Either DBError [ActivityModel])
findActivityAll conn = do
    result <- try $ query_ conn "SELECT * FROM activities"  
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right acts -> return $ Right acts

insertActivity :: Connection -> ActivityModel -> IO (Either DBError UUID)
insertActivity conn act = do
    uuid <- nextRandom
    let query = "INSERT INTO activities (content,date,user_id,id) VALUES (?,?,?,?)"
    result <- try $ execute conn query
        ( act.activityContent, 
        act.activityDate, 
        act.activityUserId, 
        uuid) :: IO (Either SQLError ())
    case result of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting activity")
        Right _ -> return $ Right uuid