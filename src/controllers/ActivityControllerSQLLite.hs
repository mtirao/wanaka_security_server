{-# LANGUAGE OverloadedStrings #-}

module ActivityControllerSQLLite(createActivity, getActivity, getActivityAll) where

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
import ActivitySQLLite
import ActivityModel
import Control.Monad.Trans.Class (MonadTrans(lift))


--- ACTIVITY
getActivity msgId conn = do
    result <- liftIO $ findActivity msgId conn
    case result of
        [] -> do
                jsonResponse (ErrorMessage "Activity not found")
                status notFound404
        [a] -> do
                status ok200
                --jsonResponse $ toActivityDTO a


getActivityAll conn = do
    result <- liftIO $ findActivityAll conn
    case result of
        [] -> do
            jsonResponse (ErrorMessage "No activities found")
            status notFound404
        activities -> do
            status ok200
            --jsonResponse $ map toActivityDTO activities

createActivity conn =  do
    bodyContent <- body
    let messageRequest = decode bodyContent :: Maybe ActivityModel
    case messageRequest of
        Just req -> do
            currentTime <- liftIO  getCurrentTime
            let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
            let activity = ActivityModel (activityContent req) posixTime (activityUserId req) Nothing
            uuid <- liftIO $ insertActivity conn activity
            jsonResponse $ SuccessMessage $ pack $ toString uuid
            status status201
               
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid activity request format")
            status badRequest400
    

