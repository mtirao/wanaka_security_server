{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ActivityModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)


-- Activity Request
data ActivityModel = ActivityModel
    { activityContent :: Text
    , activityDate :: Int64
    , activityUserId :: UUID
    , activityId :: Maybe UUID
    } deriving (Show)

instance FromJSON ActivityModel where
    parseJSON (Object v) = ActivityModel <$>
        v .: "activityContent" <*>
        v .: "activityDate" <*>
        v .: "activityUserId" <*>
        v .:? "activityId"


instance ToJSON ActivityModel where
    toJSON (ActivityModel {..}) = object
        [ "activityId" .= activityId
        , "activityContent" .= activityContent
        , "activityDate" .= activityDate
        , "activityUserId" .= activityUserId
        ]
                