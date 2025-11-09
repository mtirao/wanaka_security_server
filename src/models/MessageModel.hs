{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# language DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import SQLModel

data MessageType = Info | Fail | Warn | Alert
    deriving (Eq, Enum, Bounded, Generic)
    deriving stock (Read, Show)


-- Message
data MessageModel = MessageModel
    { messageContent :: Text
    ,  messageDate :: Int64
    ,  messageType :: MessageType
    , messageId :: Maybe UUID
    } deriving (Show)
    

instance FromJSON MessageType where
    parseJSON = withText "MessageType" $ \t -> case t of
        "INFO"  -> pure Info
        "ERROR" -> pure Fail
        "WARN"  -> pure Warn
        "ALERT" -> pure Alert
        _       -> fail $ "Unknown MessageType: " ++ show t

instance ToJSON MessageType where
    toJSON Info  = String "INFO"
    toJSON Fail = String "ERROR"
    toJSON Warn  = String "WARN"
    toJSON Alert = String "ALERT"


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
fromMessageType Info  = "INFO"
fromMessageType Fail = "ERROR"
fromMessageType Warn  = "WARN"
fromMessageType Alert = "ALERT"

toMessageType :: Text -> Maybe MessageType
toMessageType "INFO"  = Just Info
toMessageType "ERROR" = Just Fail
toMessageType "WARN"  = Just Warn
toMessageType "ALERT" = Just Alert
toMessageType _       = Nothing


-- Add FromField instance for MessageType
instance FromField MessageType where
    fromField f = do
        txt <- fromField f :: Ok Text
        case txt of
            "INFO"    -> return Info
            "WARNING" -> return Warn
            "FAIL"   -> return Fail
            "ALERT"   -> return Alert
            _        -> fail "Invalid MessageType in database"

-- Add ToField instance for MessageType
instance ToField MessageType where
    toField Info    = toField ("INFO" :: Text)
    toField Warn    = toField ("WARNING" :: Text)
    toField Fail    = toField ("FAIL" :: Text)
    toField Alert   = toField ("ALERT" :: Text)


instance FromRow MessageModel where
    fromRow = MessageModel <$> field <*> field <*> field <*> field

instance ToRow MessageModel where
    toRow m = toRow (messageContent m, messageDate m, messageType m, messageId m)