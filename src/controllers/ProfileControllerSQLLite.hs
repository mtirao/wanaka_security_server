{-# LANGUAGE OverloadedStrings #-}

module ProfileControllerSQLLite where

import ProfileModel

import Views ( jsonResponse )
import ProfileSQLLite 

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class

import qualified Data.Text.Lazy as TL

import qualified Data.ByteString.Lazy.Internal as BL 
import qualified Data.Text as T

import Data.Time
import Data.Time.Clock.POSIX
import Data.UUID.V4 (nextRandom)
import Data.Maybe
import Data.Aeson
import GHC.Int

import Network.HTTP.Types.Status

import ErrorMessage


--- PROFILE
getProfile conn =  do
        userId <- header "x-user-id"
        case userId of
            Nothing -> do
                jsonResponse (ErrorMessage "User ID not provided")
                status badRequest400
            Just uid -> do
                let uid' = TL.toStrict uid
                liftIO $ print ("Getting profile for user: " <> show uid')
                result <- liftIO $ findProfile conn uid'
                case result of
                    [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status notFound404
                    [a] -> status ok200
                        --jsonResponse $ toProfileDTO a
                                                 

createProfile body conn =  do
        b <- body
        uuid <- liftIO nextRandom
        let profileId = Just $ T.pack $ show uuid
        let profile = (decode b :: Maybe ProfileModel) 
        case profile of
            Nothing -> status badRequest400
            Just a -> do                   
                result <- liftIO $ insertProfile conn a (fromMaybe "" profileId)
                status noContent204
                                                

-- deleteUserProfile conn =  do
--         userId <- header "x-user-id"
--         case userId of
--             Nothing -> do
--                 jsonResponse (ErrorMessage "User ID not provided")
--                 status badRequest400
--             Just uid -> do
--                 let uid' = TL.toStrict uid
--                 result <- liftIO $ deleteProfile uid' conn
--                 case result of
--                         [] -> do
--                                 jsonResponse (ErrorMessage "User not found")
--                                 status notFound404
--                         [a] -> status noContent204
                                                

-- updateUserProfile body conn =  do
--         userId <- header "x-user-id"
--         case userId of
--             Nothing -> do
--                 jsonResponse (ErrorMessage "User ID not provided")
--                 status badRequest400
--             Just uid -> do
--                 let uid' = TL.toStrict uid
--                 b <- body
--                 let profile = (decode b :: Maybe ProfileModel)
--                 case profile of
--                     Nothing -> status badRequest400
--                     Just p -> do
--                         result <- liftIO $ updateProfile uid' p conn
--                         case result of
--                             Right [] -> do
--                                 jsonResponse (ErrorMessage "User not found")
--                                 status notFound404
--                             Right [a] -> status noContent204
