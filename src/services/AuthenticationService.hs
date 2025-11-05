{-# LANGUAGE OverloadedStrings #-}

module AuthenticationService(userAuthenticate) where


import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T
import qualified Web.Scotty as WS

import Control.Monad.IO.Class
import TenantsModel
import Tenant
import JwtAuth
import Views
import ErrorMessage
import SQLModel


userAuthenticate conn =  do
        h <- WS.header "Authorization"
        liftIO $ print ("Headers: " <> show h)
        case h >>= extractBasicAuth . T.encodeUtf8 . TL.toStrict of
            Just (user, password) -> do
                let userText = T.decodeUtf8 user
                let passwordText = T.decodeUtf8 password
                liftIO $ print ("User: " <> show userText)
                result <- liftIO $ findTenantByUsername conn userText
                case result of
                    Left _ -> WS.status forbidden403
                    Right a -> if userPassword a /= passwordText
                        then WS.status forbidden403
                        else do
                            liftIO $ print ("Authenticated user: " <> show userText)
                            createJwt conn $ uuidToText (userId a)
            Nothing -> WS.status unauthorized401 