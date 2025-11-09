{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ZoneModel where    

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



data ZoneState = Enabled | Disabled
    deriving (Eq, Enum, Bounded, Show)


-- Zone
data ZoneModel = ZoneModel
    { zoneState :: ZoneState
    , zoneName :: Text
    , zoneId :: Maybe UUID
    } deriving (Show)

instance FromJSON ZoneState where
    parseJSON = withText "zoneState" $ \t -> case t of
        "ENABLED"   -> pure Enabled
        "DISABLED" -> pure Disabled

instance ToJSON ZoneState where
    toJSON Enabled   = String "ENABLED"
    toJSON Disabled   = String "DISABLED"


instance FromJSON ZoneModel where
    parseJSON (Object v) = ZoneModel <$>
        v .: "state" <*>     
        v .: "name" <*>
        v .:? "zoneId"
        
instance ToJSON ZoneModel where
    toJSON (ZoneModel {..}) = object
        [ "state" .= zoneState
        , "name" .= zoneName
        , "id" .= zoneId
        ]   

instance FromField ZoneState where
    fromField f = do
        txt <- fromField f :: Ok Text
        case txt of
            "ENABLED"   -> pure Enabled
            "DISABLED" -> pure Disabled
            _        -> fail $ "Unknown SensorType: " ++ show txt

instance ToField ZoneState where
    toField Enabled   = toField ("ENABLED" :: Text)
    toField Disabled = toField ("DISABLED" :: Text)



instance FromRow ZoneModel where
    fromRow = ZoneModel <$> field <*> field <*> field

instance ToRow ZoneModel where
    toRow m = toRow (zoneState m, zoneName m, zoneId m)
