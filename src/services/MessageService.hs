{-# LANGUAGE OverloadedStrings #-}

module MessageService(createMessage, getMessage, getMessageAll, getSystemStatus) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID, toString)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Lens.Internal.TH (prismTypeName)

import Database.SQLite.Simple (Connection)

import GHC.Int

import ErrorMessage
import Views
import Message
import Activity

import MessageModel
import SQLModel

--- MESSAGE

getSystemStatus conn = do
    --armed <- liftIO $ isSystemArmed conn
    let armed = True -- TEMPORARY FIX UNTIL IMPLEMENTING SYSTEM STATUS IN 
    if armed
        then do
            jsonResponse (SuccessMessage "System is armed")
            status ok200
        else do
            jsonResponse (ErrorMessage "System is not armed")
            status badRequest400
    
getMessageAll conn = do
    liftIO $ print "Fetching all messages"
    result <- liftIO $ findMessageAll conn
    case result of
        Left err -> do
                jsonResponse (ErrorMessage "Messages not found")
                status notFound404
        Right msgs -> do
                status ok200
                jsonResponse msgs

getMessage u conn = do
    result <- liftIO $ findMessage conn u 
    case result of
        Left (MessageNotFound id) -> do
                jsonResponse (ErrorMessage "Message not found")
                status notFound404
        Left (DatabaseError err) -> do
                status status500
                jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
        Right a -> do
                status ok200
                jsonResponse $ a

createMessage conn =  do
    bodyContent <- body
    let messageRequest = decode bodyContent :: Maybe MessageModel
    case messageRequest of
        Just req -> do
            currentTime <- liftIO  getCurrentTime
            let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
            let message = MessageModel (messageContent req) posixTime (messageType req) Nothing
            result <- liftIO $ insertMessage conn message
            case result of
                Left (DatabaseError err) -> do
                    status status500
                    jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
                Right uuid -> do
                    --systemArmed <- liftIO $ isSystemArmed conn
                    let systemArmed = True -- TEMPORARY FIX UNTIL IMPLEMENTING SYSTEM STATUS IN  
                    if systemArmed
                    then do
                        liftIO $ putStrLn $ "Alerting system with message type: " ++ show req.messageType
                        jsonResponse $ SuccessMessage $ pack $ toString uuid
                        status status201
                    else do
                        jsonResponse (ErrorMessage "System is not armed")
              
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid message request format")
            status badRequest400

    
