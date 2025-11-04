{-# LANGUAGE OverloadedStrings #-}

module ActivitySQLLite where

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



findActivity :: Connection -> UUID -> IO [ActivityModel]
findActivity conn id = 
    query conn "SELECT * FROM activities WHERE id = ?" (Only id)

findActivityAll :: Connection -> IO [ActivityModel]
findActivityAll conn = 
    query_ conn "SELECT * FROM activities"  

insertActivity :: Connection -> ActivityModel -> IO UUID
insertActivity conn act = do
    uuid <- nextRandom
    execute conn "INSERT INTO activities (activity, date, user_id, id) VALUES (?,?,?,?)"
        (act.activityContent, act.activityDate, act.activityUserId, uuid)
    return uuid