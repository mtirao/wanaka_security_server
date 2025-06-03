{-# LANGUAGE OverloadedStrings #-}

module ProfileController(getProfile, createProfile, deleteUserProfile, updateUserProfile) where

import ProfileModel

import Views ( jsonResponse )
import Profile 

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
                result <- liftIO $ findProfile uid' conn
                case result of
                    Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status notFound404
                    Right [a] ->
                        jsonResponse $ toProfileDTO a
                                                 

createProfile body conn =  do
        b <- body
        uuid <- liftIO nextRandom
        let profileId = Just $ T.pack $ show uuid
        let profile = (decode b :: Maybe ProfileModel) 
        case profile of
            Nothing -> status badRequest400
            Just a -> do                   
                result <- liftIO $ insertProfile a (fromMaybe "" profileId) conn
                case result of
                    Right [] -> do
                        jsonResponse (ErrorMessage "User can be created")
                        status forbidden403
                    Right [a] -> status noContent204
                                                

deleteUserProfile conn =  do
        userId <- header "x-user-id"
        case userId of
            Nothing -> do
                jsonResponse (ErrorMessage "User ID not provided")
                status badRequest400
            Just uid -> do
                let uid' = TL.toStrict uid
                result <- liftIO $ deleteProfile uid' conn
                case result of
                        Right [] -> do
                                jsonResponse (ErrorMessage "User not found")
                                status notFound404
                        Right [a] -> status noContent204
                                                

updateUserProfile body conn =  do
        userId <- header "x-user-id"
        case userId of
            Nothing -> do
                jsonResponse (ErrorMessage "User ID not provided")
                status badRequest400
            Just uid -> do
                let uid' = TL.toStrict uid
                b <- body
                let profile = (decode b :: Maybe ProfileModel)
                case profile of
                    Nothing -> status badRequest400
                    Just p -> do
                        result <- liftIO $ updateProfile uid' p conn
                        case result of
                            Right [] -> do
                                jsonResponse (ErrorMessage "User not found")
                                status notFound404
                            Right [a] -> status noContent204
