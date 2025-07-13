{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ActivityModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)

data ActivityContent = Armed | ArmedStay | ArmedAway | ArmeCustom | Disarmed deriving (Show, Enum)
instance FromJSON ActivityContent where
    parseJSON = withText "ActivityContent" $ \t -> case t of
        "armed"       -> pure Armed
        "armed_stay"  -> pure ArmedStay
        "armed_away"  -> pure ArmedAway
        "armed_custom"-> pure ArmeCustom
        "disarmed"  -> pure Disarmed -- Assuming disarmed is treated as Armed
        _             -> fail $ "Unknown ActivityContent: " ++ show t

instance ToJSON ActivityContent where
    toJSON Armed       = String "armed"
    toJSON ArmedStay  = String "armed_stay"
    toJSON ArmedAway  = String "armed_away"
    toJSON ArmeCustom = String "armed_custom" 
    toJSON Disarmed   = String "disarmed"

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

fromMessageType :: ActivityContent -> Text  
fromMessageType Armed       = "armed"
fromMessageType ArmedStay  = "armed_stay"
fromMessageType ArmedAway  = "armed_away"
fromMessageType ArmeCustom = "armed_custom"        

toMessageType :: Text -> Maybe ActivityContent
toMessageType "armed"       = Just Armed
toMessageType "armed_stay"  = Just ArmedStay
toMessageType "armed_away"  = Just ArmedAway
toMessageType "armed_custom"= Just ArmeCustom
toMessageType _             = Nothing        