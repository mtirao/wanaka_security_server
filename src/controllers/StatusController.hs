
{-# LANGUAGE OverloadedStrings #-}

module StatusController(createStatus, getStatus) where

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
import Status
import StatusModel
import Control.Monad.Trans.Class (MonadTrans(lift))


--- MESSAGE

getStatus msgId conn = do
                            result <- liftIO $ findStatus msgId conn
                            case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Status not found")
                                            status notFound404
                                    Right [a] -> do
                                            jsonResponse $ toStatusDTO a
    

createStatus conn =  do
    bodyContent <- body
    let messageRequest = decode bodyContent :: Maybe StatusModel
    case messageRequest of
        Just req -> do
            currentTime <- liftIO  getCurrentTime
            let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
            let statuss = StatusModel (statusStatus req) posixTime (statusUserId req) Nothing
            result <- liftIO $ insertStatus statuss conn
            case result of
                Right [uuid] -> do
                    jsonResponse $ SuccessMessage $ pack $ toString uuid
                    status status201
                Left err -> do
                    jsonResponse (ErrorMessage "Error inserting status")
                    status internalServerError500
        Nothing -> do
            -- If the body cannot be parsed as MessageRequest, return an error
            jsonResponse (ErrorMessage "Invalid status request format")
            status badRequest400
    

