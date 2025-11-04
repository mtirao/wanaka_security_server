{-# LANGUAGE OverloadedStrings #-}

module Message where

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
import Control.Exception (try)


findMessage :: Connection -> UUID -> IO (Either DBError MessageModel)
findMessage conn id = do
    result <- try $ query conn "SELECT * FROM messages WHERE id = ?" (Only id)
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right [] -> return $ Left $ MessageNotFound id
        Right [profile] -> return $ Right profile
        Right _ -> return $ Left $ DatabaseError (SQLError ErrorError "2" "Multiple profiles found for single ID")


findMessageAll :: Connection -> IO (Either DBError [MessageModel])
findMessageAll conn = do
    result <- try $ query_ conn "SELECT * FROM messages"
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right msgs -> return $ Right msgs


insertMessage :: Connection -> MessageModel -> IO (Either DBError UUID)
insertMessage conn msg = do
    uuid <- nextRandom
    let query = "INSERT INTO messages (content,date,type,id) VALUES (?,?,?,?)"
    res <- try $ execute conn query
        ( msg.messageContent
        , msg.messageDate
        , msg.messageType
        , uuid
        ) :: IO (Either SQLError ())
    case res of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting message")
        Right _ ->  return $ Right uuid