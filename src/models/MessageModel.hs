{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)


-- Message Request
data MessageRequest = MessageRequest
    { messageContent :: Text
    ,  messageDate :: Int64
    ,  messageType :: Text
    } deriving (Show)

instance FromJSON MessageRequest where
    parseJSON (Object v) = MessageRequest <$>
        v .: "messageContent" <*>
        v .: "messageDate" <*>
        v .: "messageType"

data MessageResponse = MessageResponse
    { messageId :: UUID
    , content :: Maybe Text
    , date :: Maybe Int64
    , msgType :: Maybe Text
    } deriving (Show)

instance ToJSON MessageResponse where
    toJSON (MessageResponse {..}) = object
        [ "id" .= messageId
        , "content" .= content
        , "date" .= date
        , "type" .= msgType
        ]
                