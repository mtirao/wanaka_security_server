{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RelationModel where    

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


data RelationModel = RelationModel
    {relationZoneId :: UUID
    , relationSensorId :: UUID
    } deriving (Show)


instance FromJSON RelationModel where
    parseJSON = withObject "RelationModel" $ \param -> do
        RelationModel <$> param .: "zone" <*> param .: "sensor"
        
instance ToJSON RelationModel where
    toJSON (RelationModel {..}) = object
        [ "sensor" .= relationSensorId
        , "zone" .= relationZoneId
        ]  


instance FromRow RelationModel where
    fromRow = RelationModel <$> field <*> field

instance ToRow RelationModel where
    toRow m = toRow (relationZoneId m, relationSensorId m)