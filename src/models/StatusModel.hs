{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module StatusModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import SQLModel

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

instance FromRow StatusModel where
    fromRow = StatusModel<$> field <*> field <*> field <*> field    

instance ToRow StatusModel where
    toRow m = toRow (statusStatus m, statusDate m, statusUserId m, statusId m)               