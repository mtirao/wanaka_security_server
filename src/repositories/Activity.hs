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
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text (Text)
import GHC.Int (Int64)
import ActivityModel (ActivityModel(..), ActivityContent(..))
import SQLModel
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)


findActivity :: Connection -> UUID -> IO (Either DBError ActivityModel)
findActivity conn id = do
    result <- try $ query conn "SELECT content, date, id FROM activities WHERE id = ?" (Only id)
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right [] -> return $ Left $ ActivityNotFound id
        Right [activity] -> return $ Right activity
        Right _ -> return $ Left $ DatabaseError (SQLError ErrorError "2" "Multiple activities found for single ID")


findActivityAll :: Connection -> IO (Either DBError [ActivityModel])
findActivityAll conn = do
    result <- try $ query_ conn "SELECT content, date, id FROM activities ORDER BY date DESC"  
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right acts -> return $ Right acts


deleteActivity :: Connection -> UUID -> IO (Either DBError UUID)
deleteActivity conn actid = do
    result <- try $ execute conn "DELETE FROM activities WHERE id = ?" (Only actid) :: IO (Either SQLError ())
    case result of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting activity")
        Right _ -> return $ Right actid


insertActivity :: Connection -> ActivityModel -> IO (Either DBError UUID)
insertActivity conn act = do
    uuid <- nextRandom
    currentTime <- liftIO getCurrentTime
    let query = "INSERT INTO activities (content, date, id) VALUES (?,?,?)"
    let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
    result <- try $ execute conn query
        ( act.activityContent, 
        posixTime, 
        uuid) :: IO (Either SQLError ())
    case result of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting activity")
        Right _ -> return $ Right uuid