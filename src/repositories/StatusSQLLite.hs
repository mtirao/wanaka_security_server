{-# LANGUAGE OverloadedStrings #-}

module StatusSQLLite where

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
import StatusModel (StatusModel(..))
import SQLModel


findStatus :: Connection -> UUID -> IO [StatusModel]
findStatus conn id = 
    query conn "SELECT * FROM statuses WHERE id = ?" (Only id)  

findStatusAll :: Connection -> IO [StatusModel]
findStatusAll conn = 
    query_ conn "SELECT * FROM statuses"    
    
insertStatus :: Connection -> StatusModel -> IO UUID
insertStatus conn status = do
    uuid <- nextRandom
    execute conn "INSERT INTO statuses (status, date, user_id, id) VALUES (?,?,?,?)"
        (statusStatus status, statusDate status, statusUserId status, uuid)
    return uuid