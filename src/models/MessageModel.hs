{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)

data MessageType = Info | Fail | Warn | Alert deriving (Show, Enum, Eq)

instance FromJSON MessageType where
    parseJSON = withText "MessageType" $ \t -> case t of
        "info"  -> pure Info
        "error" -> pure Fail
        "warn"  -> pure Warn
        "alert" -> pure Alert
        _       -> fail $ "Unknown MessageType: " ++ show t

instance ToJSON MessageType where
    toJSON Info  = String "info"
    toJSON Fail = String "error"
    toJSON Warn  = String "warn"
    toJSON Alert = String "alert"

-- Message Request
data MessageModel = MessageModel
    { messageContent :: Text
    ,  messageDate :: Int64
    ,  messageType :: MessageType
    , messageId :: Maybe UUID
    } deriving (Show)

instance FromJSON MessageModel where
    parseJSON (Object v) = MessageModel <$>
        v .: "messageContent" <*>
        v .: "messageDate" <*>
        v .: "messageType" <*>
        v .:? "messageId"

instance ToJSON MessageModel where
    toJSON (MessageModel {..}) = object
        [ "messageId" .= messageId
        , "messageContent" .= messageContent
        , "messageDate" .= messageDate
        , "messageType" .= messageType
        ]

fromMessageType :: MessageType -> Text     
fromMessageType Info  = "info"
fromMessageType Fail = "error"
fromMessageType Warn  = "warn"
fromMessageType Alert = "alert"

toMessageType :: Text -> Maybe MessageType
toMessageType "info"  = Just Info
toMessageType "error" = Just Fail
toMessageType "warn"  = Just Warn
toMessageType "alert" = Just Alert
toMessageType _       = Nothing