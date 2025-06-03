{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ProfileModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)


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
    , profileId :: Maybe Text  -- Assuming profileId is a UUID stored as Text
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