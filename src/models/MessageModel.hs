{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)


-- Message Request
data MessageModel = MessageModel
    { messageContent :: Text
    ,  messageDate :: Int64
    ,  messageType :: Text
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
      