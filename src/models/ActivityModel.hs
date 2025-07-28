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
import Rel8 (DBType(..), ReadShow(..))

data ActivityContent = Armed | ArmedStay | ArmedAway | ArmeCustom | Disarmed 
    deriving (Eq, Enum, Bounded, Generic)
    deriving stock (Read, Show)
    deriving DBType via ReadShow ActivityContent

instance FromJSON ActivityContent where
    parseJSON = withText "ActivityContent" $ \t -> case t of
        "Armed"       -> pure Armed
        "ArmedStay"  -> pure ArmedStay
        "ArmedAway"  -> pure ArmedAway
        "ArmedCustom"-> pure ArmeCustom
        "Disarmed"  -> pure Disarmed -- Assuming disarmed is treated as Armed
        _             -> fail $ "Unknown ActivityContent: " ++ show t

instance ToJSON ActivityContent where
    toJSON Armed       = String "Armed"
    toJSON ArmedStay  = String "ArmedStay"
    toJSON ArmedAway  = String "ArmedAway"
    toJSON ArmeCustom = String "ArmedCustom" 
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
fromContentType  ArmeCustom = "armed_custom"        

toContentType :: Text -> Maybe ActivityContent
toContentType  "armed"       = Just Armed
toContentType  "armed_stay"  = Just ArmedStay
toContentType  "armed_away"  = Just ArmedAway
toContentType  "armed_custom"= Just ArmeCustom
toContentType  _             = Nothing        