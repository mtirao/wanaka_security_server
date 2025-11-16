{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)

import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Control.Exception (finally)
import Control.Monad.IO.Class
import Control.Concurrent (forkIO, killThread)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (installHandler, keyboardSignal, sigTERM, Handler(Catch))
import System.Exit (ExitCode(ExitSuccess))

import qualified Data.Text.Lazy as TL

import MessageService
import ActivityService
import AuthenticationService
import ProfileService
import TenantService
import MqttService
import SensorService
import ZoneService
import RelationService
import JwtAuth
import Database
import Mqtt


main :: IO ()
main = do
    conn <- initDB
    let messageHandler = insertNewMessage conn
    mqttTid <- forkIO $ mqttSubscribe messageHandler

    let cleanup = do
            putStrLn "Shutting down..."
            -- stop background threads if any
            killThread mqttTid
            -- close the DB connection
            closeDB conn
            putStrLn "Cleanup complete."
            exitImmediately ExitSuccess

    -- install handlers for SIGINT (Ctrl-C) and SIGTERM
    _ <- installHandler keyboardSignal (Catch cleanup) Nothing
    _ <- installHandler sigTERM (Catch cleanup) Nothing

    scotty 3000 $ do
        middleware logStdoutDev
        middleware (jwtAuthMiddleware conn)

        -- MESSAGE
        get "/api/wanaka/message/:id" $ do
            idd <- param "id" :: ActionM TL.Text
            let uuid = read (TL.unpack idd) :: UUID
            getMessage uuid conn
        get "/api/wanaka/message" $ getMessageAll conn
        post "/api/wanaka/message" $ createMessage conn

        -- ACTIVITY
        get "/api/wanaka/activity/:id" $ do
            idd <- param "id" :: ActionM TL.Text
            let uuid = read (TL.unpack idd) :: UUID
            getActivity conn uuid
        get "/api/wanaka/activity" $ getActivityAll conn
        post "/api/wanaka/activity" $ createActivity conn
        delete "/api/wanaka/activity/:id" $ do
            idd <- param "id" :: ActionM TL.Text
            let uuid = read (TL.unpack idd) :: UUID
            removeActivity conn uuid

        -- AUTHENTICATION
        post "/api/wanaka/accounts/login" $ userAuthenticate conn
        post "/api/wanaka/accounts/tenant" $ createTenant body conn

        -- PROFILE
        get "/api/wanaka/profile" $ getProfile conn
        post "/api/wanaka/profile" $ createProfile body conn

        -- SENSOR
        get "/api/wanaka/sensor" $ getSensorAll conn
        get "/api/wanaka/sensor/zone/:zoneId" $ do
            zoneIdParam <- param "zoneId" :: ActionM TL.Text
            let zoneId = read (TL.unpack zoneIdParam) :: UUID
            getSensorByZoneId conn zoneId
        post "/api/wanaka/sensor" $ createSensor conn

        -- ZONE
        get "/api/wanaka/zone" $ getZoneAll conn
        post "/api/wanaka/zone" $ createZone conn

        -- RELATION
        get "/api/wanaka/relation" $ getRelationAll conn
        post "/api/wanaka/relation" $ createRelation conn
    