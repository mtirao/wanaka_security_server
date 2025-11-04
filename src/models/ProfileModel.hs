{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ProfileModel where

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

-- Profile
data ProfileModel = ProfileModel
    { cellPhone :: Text
    , email :: Text
    , firstName :: Text
    , lastName :: Text
    , phone :: Text
    , gender :: Text
    , address :: Text
    , city :: Text
    , profileId :: Maybe UUID  -- Assuming profileId is a UUID stored as Text
    } deriving (Show)

instance ToJSON ProfileModel where
    toJSON ProfileModel {..} = object [
            "cellphone" .= cellPhone,
            "email" .= email,
            "firstname" .= firstName,
            "lastname" .= lastName,
            "phone" .= phone,
            "gender" .= gender,
            "address" .= address,
            "city" .= city,
            "profileid" .= profileId
        ]

instance FromJSON ProfileModel where
    parseJSON (Object v) = ProfileModel <$>
        v .:  "cellphone" <*>
        v .:  "email" <*>
        v .:  "firstname" <*>
        v .:  "lastname" <*>
        v .:  "phone" <*>
        v .: "gender" <*>
        v .: "address" <*>
        v .: "city" <*>
        v .:? "profileId"

instance FromRow ProfileModel where
    fromRow = ProfileModel <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field     

instance ToRow ProfileModel where
    toRow m = toRow (cellPhone m, email m, firstName m, lastName m, phone m, gender m, address m, city m, profileId m)

