{-# LANGUAGE OverloadedStrings #-}


module MqttService(insertNewMessage) where


import Data.Time (getCurrentTime)
import Data.Text (Text)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)

import Database.SQLite.Simple (Connection)

import Message (insertMessage)
import MessageModel (MessageModel(..), MessageType(..))
import ActivityService
import SQLModel (DBError(..))




insertNewMessage :: Connection -> MessageModel -> IO (Either DBError Text)
insertNewMessage conn req =  do
    currentTime <- liftIO  getCurrentTime
    let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
    let message = MessageModel (messageContent req) posixTime (messageType req) Nothing
    result <- liftIO $ insertMessage conn message
    case result of
        Left (DatabaseError err) -> do
            return $ Left $ DatabaseError err
        Right uuid -> do
            isArmed <- liftIO $ isSystemArmed conn
            if isArmed && (messageType req == Alert) then do
                putStrLn "System is armed and alert message received. Triggering alert..."
                return $ Right "System is armed. Alert triggered." 
            else
                return $ Right "System is disarmed. No alert triggered." 