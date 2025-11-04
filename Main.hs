{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)

import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Control.Monad.IO.Class
import Control.Concurrent( forkIO, killThread)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (installHandler, keyboardSignal, sigTERM, sigKILL, Handler(Catch))
import System.Exit (ExitCode(ExitSuccess))

import qualified Data.Text.Lazy as TL

import MessageControllerSQLLite
import ActivityControllerSQLLite
import AuthenticationControllerSQLLite
import ProfileControllerSQLLite
import JwtAuth
import Database
import Mqtt



main :: IO ()
main = do
        conn <- initDB
        -- mqttTid <- forkIO $ mqttSubscribe conn
        let cleanup = do
                putStrLn "Shutting down..."
                -- stop background threads if any
                --killThread mqttTid
                -- release DB connection (Hasql.Connection.release)
                -- S.release conn
                putStrLn "Cleanup complete."
                exitImmediately ExitSuccess
            -- install handlers for SIGINT (Ctrl-C) and SIGTERM
        _ <- installHandler keyboardSignal (Catch cleanup) Nothing
        _ <- installHandler sigTERM (Catch cleanup) Nothing

        scotty 3000 $ do
            middleware logStdoutDev
            --middleware (jwtAuthMiddleware conn)
            -- MESSAGE
            get "/api/wanaka/message/:id" $ do
                idd <- param "id" :: ActionM TL.Text
                let uuid = read (TL.unpack idd) :: UUID
                getMessage conn uuid
            get "/api/wanaka/message" $ getMessageAll conn
            post "/api/wanaka/message" $ createMessage conn

            -- ACTIVITY
            get "/api/wanaka/activity/:id" $ do
                idd <- param "id" :: ActionM TL.Text
                let uuid = read (TL.unpack idd) :: UUID
                getActivity conn uuid
            get "/api/wanaka/activity" $ getActivityAll conn
            post "/api/wanaka/activity" $ createActivity conn

            -- SYSTEM STATUS
            get "/api/wanaka/status" $ getSystemStatus conn

            -- AUTHENTICATION
            post "/api/wanaka/accounts/login" $ userAuthenticateSql conn

            -- PROFILE
            get "/api/wanaka/profile" $ getProfile conn
            post "/api/wanaka/profile" $ createProfile body conn
            --delete "/api/wanaka/profile/:id" $ deleteUserProfile conn
            --put "/api/wanaka/profile/:id" $ updateUserProfile body conn


