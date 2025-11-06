{-# LANGUAGE OverloadedStrings #-}

module Mqtt(mqttSubscribe) where

import Network.MQTT.Client
import Network.MQTT.Topic (unTopic)
import Network.URI (parseURI)
import GHC.Int
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (UUID)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Message
import MessageModel
import SQLModel
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(lift))


msgReceived clousure _ topic message property = do
    let topicName = unTopic topic  -- Convert ByteString to String
    currentTime <- getCurrentTime
    let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
    uuid <- nextRandom
    let msgType = case topicName of
                    "alert" -> Alert
                    "fail" -> Fail
                    "info"  -> Info
                    "warn"  -> Warn
                    _       -> Info
    let msgContent = MessageModel (decodeUtf8 $ BL.toStrict message) posixTime msgType (Just uuid)
    result <- clousure msgContent
    case result of
        Right id -> putStrLn $ "Message stored: " ++ show id
        _ -> putStrLn $ "Error storing message"



mqttSubscribe :: (MessageModel -> IO (Either DBError Text)) -> IO ()
mqttSubscribe clousure = do 
    let (Just uri) = parseURI "mqtt://localhost:1883"
    mc <- connectURI mqttConfig{_msgCB=SimpleCallback $ msgReceived clousure} uri
    print =<< subscribe mc [("alert", subOptions), ("fail", subOptions), ("info", subOptions), ("warn", subOptions)] []
    waitForClient mc   -- wait for the the client to disconnect

