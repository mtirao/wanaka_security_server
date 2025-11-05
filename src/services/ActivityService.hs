{-# LANGUAGE OverloadedStrings #-}

module ActivityService(createActivity, getActivity, getActivityAll, removeActivity) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Aeson (FromJSON, ToJSON, encode, decode)
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
import Activity
import ActivityModel
import Control.Monad.Trans.Class (MonadTrans(lift))
import SQLModel

--- ACTIVITY
getActivity conn msgId = do
    result <- liftIO $ findActivity conn msgId
    case result of
        Left (ActivityNotFound id) -> do
                jsonResponse (ErrorMessage "Activity not found")
                status notFound404
        Left (DatabaseError err) -> do
                status status500
                jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
        Right a -> do
                status ok200
                jsonResponse $ a

removeActivity conn msgId = do
    result <- liftIO $ deleteActivity conn msgId
    case result of
        Left (DatabaseError err) -> do
                status status500
                jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
        Right a -> do
                status ok200
                jsonResponse $ a

getActivityAll conn = do
    result <- liftIO $ findActivityAll conn
    case result of
        Left err -> do
                jsonResponse (ErrorMessage "Messages not found")
                status notFound404
        Right msgs -> do
                status ok200
                jsonResponse msgs


createActivity conn =  do
    bodyContent <- body
    let messageRequest = decode bodyContent :: Maybe ActivityModel
    case messageRequest of
        Just activity -> do
            result <- liftIO $ insertActivity conn activity
            case result of
                Left (DatabaseError err) -> do
                    status status500
                    jsonResponse $ ErrorMessage $ "Database error: " <> (pack $ show err)
                Right uuid -> do
                    jsonResponse $ SuccessMessage $ pack $ toString uuid
                    status status201
               
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid activity request format")
            status badRequest400
    

