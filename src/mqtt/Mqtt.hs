{-# LANGUAGE OverloadedStrings #-}

module Mqtt(mqttSubscribe) where

import Network.MQTT.Client
import Network.MQTT.Topic (unTopic)
import Network.URI (parseURI)
import GHC.Int
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Message
import MessageModel
import Hasql.Connection (Connection)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8, decodeUtf8)


msgReceived conn _ topic message property = do
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
    result <- insertMessage msgContent conn
    case result of
        Left err -> putStrLn $ "Error inserting message: " ++ show err
        Right _  -> putStrLn $ "Received message on topic: " ++ show topicName ++ " with payload: " ++ show message ++ " at time: " ++ show posixTime


mqttSubscribe :: Connection -> IO ()
mqttSubscribe conn = do 
    let (Just uri) = parseURI "mqtt://localhost:1883"
    mc <- connectURI mqttConfig{_msgCB=SimpleCallback $ msgReceived conn} uri
    print =<< subscribe mc [("alert", subOptions), ("fail", subOptions), ("info", subOptions), ("warn", subOptions)] []
    waitForClient mc   -- wait for the the client to disconnect