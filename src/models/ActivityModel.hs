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
        "Armed"       -> pure Armed
        "ArmedStay"  -> pure ArmedStay
        "ArmedAway"  -> pure ArmedAway
        "ArmedCustom"-> pure ArmedCustom
        "Disarmed"  -> pure Disarmed -- Assuming disarmed is treated as Armed
        _             -> fail $ "Unknown ActivityContent: " ++ show t

instance ToJSON ActivityContent where
    toJSON Armed       = String "Armed"
    toJSON ArmedStay  = String "ArmedStay"
    toJSON ArmedAway  = String "ArmedAway"
    toJSON ArmedCustom = String "ArmedCustom" 
    toJSON Disarmed   = String "Disarmed"

-- Activity Request
data ActivityModel = ActivityModel
    { activityContent :: ActivityContent
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

fromContentType :: ActivityContent -> Text  
fromContentType  Armed       = "armed"
fromContentType  ArmedStay  = "armed_stay"
fromContentType  ArmedAway  = "armed_away"
fromContentType  ArmedCustom = "armed_custom"        

toContentType :: Text -> Maybe ActivityContent
toContentType  "armed"       = Just Armed
toContentType  "armed_stay"  = Just ArmedStay
toContentType  "armed_away"  = Just ArmedAway
toContentType  "armed_custom"= Just ArmedCustom
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
    fromRow = ActivityModel <$> field <*> field <*> field <*> field

instance ToRow ActivityModel where
    toRow m = toRow (activityContent m, activityDate m, activityUserId m, activityId m)