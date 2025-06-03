{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module StatusModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)


-- Activity Request
data StatusModel = StatusModel
    { statusStatus :: Text
    , statusDate :: Int64
    , statusUserId :: UUID
    , statusId :: Maybe UUID
    } deriving (Show)

instance FromJSON StatusModel where
    parseJSON (Object v) = StatusModel <$>
        v .: "statusStatus" <*>
        v .: "statusDate" <*>
        v .: "statusUserId" <*>
        v .:? "statusId"


instance ToJSON StatusModel where
    toJSON (StatusModel {..}) = object
        [ "statusId" .= statusId
        , "statusStatus" .= statusStatus
        , "statusDate" .= statusDate
        , "statusUserId" .= statusUserId
        ]
                