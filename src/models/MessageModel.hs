{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int


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
                