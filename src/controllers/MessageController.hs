{-# LANGUAGE OverloadedStrings #-}

module MessageController(createMessage, getMessage, getMessageAll, getSystemStatus) where

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
import GHC.Int
import ErrorMessage
import Views
import Message
import MessageModel
import Control.Monad.Trans.Class (MonadTrans(lift))

import Activity
import Hasql.Decoders (custom)
import Control.Lens.Internal.TH (prismTypeName)

--- MESSAGE

getSystemStatus conn = do
    armed <- liftIO $ isSystemArmed conn
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
                                Right [] -> do
                                        jsonResponse (ErrorMessage "Messages not found")
                                        status notFound404
                                Right a -> do
                                        jsonResponse $ map toMessageDTO a

getMessage u conn = do
                        result <- liftIO $ findMessage u conn
                        case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status notFound404
                                Right [a] -> do
                                        jsonResponse $ toMessageDTO a
     

createMessage conn =  do
    bodyContent <- body
    let messageRequest = decode bodyContent :: Maybe MessageModel
    case messageRequest of
        Just req -> do
            currentTime <- liftIO  getCurrentTime
            let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
            let message = MessageModel (messageContent req) posixTime (messageType req) Nothing
            result <- liftIO $ insertMessage message conn
            case result of
                Right [uuid] -> do 
                    systemArmed <- liftIO $ isSystemArmed conn
                    if systemArmed
                    then do
                        liftIO $ putStrLn $ "Alerting system with message type: " ++ show req.messageType
                        jsonResponse $ SuccessMessage $ pack $ toString uuid
                        status status201
                    else do
                        jsonResponse (ErrorMessage "System is not armed")
                        status status201
                Left err -> do
                    jsonResponse (ErrorMessage "Error inserting message ")
                    status internalServerError500
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid message request format")
            status badRequest400
    
