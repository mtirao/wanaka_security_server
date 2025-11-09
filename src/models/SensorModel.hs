{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module SensorModel where    

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

-- Sensor Types and Statuses
data SensorType = DoorSensor | WindowSensor | MotionSensor | SmokeSensor
    deriving (Eq, Enum, Bounded, Show)

data SensorStatus = Active | Inactive | Triggered | Malfunctioning
    deriving (Eq, Enum, Bounded, Show)

-- Sensor
data SensorModel = SensorModel
    { sensorStatus:: SensorStatus
    , sensorType :: SensorType
    , sensorLocation :: Text
    , sensorId :: Maybe UUID
    } deriving (Show)


instance FromJSON SensorType where
    parseJSON = withText "SensorType" $ \t -> case t of
        "DOOR"   -> pure DoorSensor
        "WINDOW" -> pure WindowSensor
        "MOTION" -> pure MotionSensor
        "SMOKE"  -> pure SmokeSensor
        _               -> fail $ "Unknown SensorType: " ++ show t

instance ToJSON SensorType where
    toJSON DoorSensor   = String "DOOR"
    toJSON WindowSensor = String "WINDOW"
    toJSON MotionSensor = String "MOTION"
    toJSON SmokeSensor  = String "SMOKE"


instance FromJSON SensorStatus where
    parseJSON = withText "SensorStatus" $ \t -> case t of
        "ACTIVE"        -> pure Active
        "INACTIVE"      -> pure Inactive
        "TRIGGERED"     -> pure Triggered
        "MALFUNCTIONING"-> pure Malfunctioning
        _               -> fail $ "Unknown SensorStatus: " ++ show t   

instance ToJSON SensorStatus where
    toJSON Active        = String "ACTIVE"
    toJSON Inactive      = String "INACTIVE"
    toJSON Triggered     = String "TRIGGERED"   
    toJSON Malfunctioning= String "MALFUNCTIONING"


instance FromJSON SensorModel where
    parseJSON (Object v) = SensorModel <$>
        v .: "status" <*>     
        v .: "type" <*>
        v .: "location" <*>
        v .:? "id"

instance ToJSON SensorModel where
    toJSON (SensorModel {..}) = object
        [ "id" .= sensorId
        , "type" .= sensorType
        , "location" .= sensorLocation
        , "status" .= sensorStatus
        ]   

-- Add FromField instance for SensorType
instance FromField SensorType where
    fromField f = do
        txt <- fromField f :: Ok Text
        case txt of
            "DOOR"   -> pure DoorSensor
            "WINDOW" -> pure WindowSensor
            "MOTION" -> pure MotionSensor
            "SMOKE"  -> pure SmokeSensor
            _        -> fail $ "Unknown SensorType: " ++ show txt

-- Add FromField instance for SensorStatus
instance FromField SensorStatus where
    fromField f = do
        txt <- fromField f :: Ok Text
        case txt of
            "ACTIVE"        -> pure Active
            "INACTIVE"      -> pure Inactive
            "TRIGGERED"     -> pure Triggered   
            "MALFUNCTIONING"-> pure Malfunctioning
            _               -> fail $ "Unknown SensorStatus: " ++ show txt  
-- Add ToField instance for SensorType
instance ToField SensorType where
    toField DoorSensor   = toField ("DOOR" :: Text)
    toField WindowSensor = toField ("WINDOW" :: Text)
    toField MotionSensor = toField ("MOTION" :: Text)
    toField SmokeSensor  = toField ("SMOKE" :: Text)  


-- Add ToField instance for SensorStatus
instance ToField SensorStatus where
    toField Active        = toField ("ACTIVE" :: Text)          
    toField Inactive      = toField ("INACTIVE" :: Text)
    toField Triggered     = toField ("TRIGGERED" :: Text)   
    toField Malfunctioning= toField ("MALFUNCTIONING" :: Text)  

instance FromRow SensorModel where
    fromRow = SensorModel <$> field <*> field <*> field <*> field

instance ToRow SensorModel where
    toRow m = toRow (sensorStatus m, sensorType m, sensorLocation m, sensorId m)

 