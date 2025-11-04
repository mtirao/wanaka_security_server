{-# LANGUAGE OverloadedStrings #-}

module MessageSQLLite where

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
import MessageModel (MessageModel(..), MessageType(..))
import SQLModel



findMessage :: Connection -> UUID -> IO [MessageModel]
findMessage conn id = 
    query conn "SELECT * FROM messages WHERE id = ?" (Only id)

findMessageAll :: Connection -> IO [MessageModel]
findMessageAll conn = 
    query_ conn "SELECT * FROM messages"

insertMessage :: Connection -> MessageModel -> IO UUID
insertMessage conn msg = do
    uuid <- nextRandom
    execute conn "INSERT INTO messages (content,date,type,id) VALUES (?,?,?,?)"
           (msg.messageContent, msg.messageDate, msg.messageType, uuid)
    return uuid