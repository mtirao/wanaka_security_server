{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)

import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Control.Monad.IO.Class
import Control.Concurrent (forkIO)

import qualified Data.Text.Lazy as TL
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
--import Hasql.Pool (Pool, acquire, use, release)
import qualified Hasql.Connection as S
import Hasql.Session (Session)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import MessageController
import ActivityController
import AuthenticationController
import ProfileController
import JwtAuth
import Mqtt
import RpiDevice



data DbConfig = DbConfig
    { dbName     :: String
    , dbUser     :: String
    , dbPassword :: String
    , dbHost     :: String
    , dbPort     :: Int
    }


makeDbConfig :: C.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    dbConfPort <- C.lookup conf "database.port" :: IO (Maybe Int)
    return $ DbConfig <$> dbConfname
                      <*> dbConfUser
                      <*> dbConfPassword
                      <*> dbConfHost
                      <*> dbConfPort
                

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "Error loading configuration"
        Just conf -> do
            let connSettings = S.settings (encodeUtf8 $ pack $ dbHost conf)
                                        (fromIntegral $ dbPort conf)
                                        (encodeUtf8 $ pack $ dbUser conf)
                                        (encodeUtf8 $ pack $ dbPassword conf)
                                        (encodeUtf8 $ pack $ dbName conf)
            result <- S.acquire connSettings
            case result of
                Left err -> putStrLn $ "Error acquiring connection: " ++ show err
                Right conn -> do 
                    _ <- forkIO $ mqttSubscribe conn
                    _ <- gpioWarn False
                    _ <- gpioInfo True
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
                            getActivity uuid conn
                        get "/api/wanaka/activity" $ getActivityAll conn
                        post "/api/wanaka/activity" $ createActivity conn

                        -- SYSTEM STATUS
                        get "/api/wanaka/status" $ getSystemStatus conn

                        -- AUTHENTICATION
                        post "/api/wanaka/accounts/login" $ userAuthenticate conn

                        -- PROFILE
                        get "/api/wanaka/profile" $ getProfile conn
                        post "/api/wanaka/profile" $ createProfile body conn
                        delete "/api/wanaka/profile/:id" $ deleteUserProfile conn
                        put "/api/wanaka/profile/:id" $ updateUserProfile body conn


