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
    } deriving (Show)

instance ToJSON MessageResponse where
    toJSON (MessageResponse {..}) = object
        [ "messageId" .= messageId
        ]
                