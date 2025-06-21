{-# LANGUAGE OverloadedStrings #-}

module AuthenticationController(userAuthenticate) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T

import GHC.Int

import Control.Monad.IO.Class
import TenantsModel
import Tenant
import Realm
import JwtAuth
import Views
import ErrorMessage


userAuthenticate conn =  do
        h <- header "Authorization"
        maybeClientId<- header "x-client-id"
        maybeClientSecret <- header "x-client-secret"
        maybeGrantType <- header "x-grant-type"
        case h >>= extractBasicAuth . T.encodeUtf8 . TL.toStrict of
            Just (user, password) -> do
                let userText = T.decodeUtf8 user
                let passwordText = T.decodeUtf8 password
                result <- liftIO $ findTenant userText passwordText conn
                case result of
                    Left e -> do
                            liftIO $ print result
                            jsonResponse (ErrorMessage "User not found")
                            status forbidden403
                    Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                    Right [tenant] -> case (maybeClientId, maybeClientSecret, maybeGrantType) of
                            (Just a, Just b, Just c) -> do 
                                result <- liftIO $ findRealm (TL.toStrict b) conn
                                case result of
                                    Left _ -> do
                                        jsonResponse (ErrorMessage "Error creating token")
                                        status internalServerError500
                                    Right realm -> case realm of 
                                        [] -> do
                                            jsonResponse (ErrorMessage "Invalid client secret")
                                            status unauthorized401
                                        [r] -> do
                                            let grantType = getGrantType r
                                            let clientid = toTenantDTO tenant
                                            if TL.toStrict a == getClientId r then 
                                                createJwt conn clientid.userid grantType
                                            else do
                                                jsonResponse (ErrorMessage "Invalid client secret")
                                                status unauthorized401
                            _ -> do
                                jsonResponse (ErrorMessage "Invalid request")
                                status badRequest400
            Nothing -> status unauthorized401