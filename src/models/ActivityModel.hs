{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# language DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ActivityModel where

import GHC.Generics (Generic)
import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)
import Network.Wreq.Types (ContentType)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import SQLModel

data ActivityContent = Armed | ArmedStay | ArmedAway | ArmedCustom | Disarmed 
    deriving (Eq, Enum, Bounded, Generic)
    deriving stock (Read, Show)

instance FromJSON ActivityContent where
    parseJSON = withText "ActivityContent" $ \t -> case t of
        "ARMED"  -> pure Armed
        "STAY"  -> pure ArmedStay
        "AWAY"  -> pure ArmedAway
        "CUSTOM"-> pure ArmedCustom
        "DISARMED"  -> pure Disarmed -- Assuming disarmed is treated as Armed
        _             -> fail $ "Unknown ActivityContent: " ++ show t

instance ToJSON ActivityContent where
    toJSON Armed       = String "ARMED"
    toJSON ArmedStay  = String "STAY"
    toJSON ArmedAway  = String "AWAY"
    toJSON ArmedCustom = String "CUSTOM" 
    toJSON Disarmed   = String "DISARMED"

-- Activity Request
data ActivityModel = ActivityModel
    { activityContent :: ActivityContent
    , activityDate :: Maybe Int64
    , activityId :: Maybe UUID
    } deriving (Show)

instance FromJSON ActivityModel where
    parseJSON (Object v) = ActivityModel <$>
        v .: "activityContent" <*>
        v .:? "activityDate" <*>
        v .:? "activityId"


instance ToJSON ActivityModel where
    toJSON (ActivityModel {..}) = object
        [ "activityId" .= activityId
        , "activityContent" .= activityContent
        , "activityDate" .= activityDate
        ]

fromContentType :: ActivityContent -> Text  
fromContentType  Armed       = "ARMED"
fromContentType  ArmedStay  = "STAY"
fromContentType  ArmedAway  = "AWAY"
fromContentType  ArmedCustom = "CUSTOM"        

toContentType :: Text -> Maybe ActivityContent
toContentType  "ARMED"       = Just Armed
toContentType  "STAY"  = Just ArmedStay
toContentType  "AWAY"  = Just ArmedAway
toContentType  "CUSTOM"= Just ArmedCustom
toContentType  _             = Nothing        


-- Add FromField instance for ActivityContent
instance FromField ActivityContent where
    fromField f = do
        txt <- fromField f :: Ok Text
        case txt of
            "ARMED"    -> return Armed
            "STAY" -> return ArmedStay
            "AWAY"   -> return ArmedAway
            "CUSTOM"   -> return ArmedCustom
            "DISARMED"   -> return Disarmed
            _        -> fail "Invalid ActivityContent in database"

-- Add ToField instance for ActivityContent
instance ToField ActivityContent  where
    toField Armed    = toField ("ARMED" :: Text)
    toField ArmedStay    = toField ("STAY" :: Text)
    toField ArmedAway    = toField ("AWAY" :: Text)
    toField ArmedCustom   = toField ("CUSTOM" :: Text)
    toField Disarmed   = toField ("DISARMED" :: Text)

instance FromRow ActivityModel where
    fromRow = ActivityModel <$> field <*> field <*> field

instance ToRow ActivityModel where
    toRow m = toRow (activityContent m, activityDate m, activityId m)