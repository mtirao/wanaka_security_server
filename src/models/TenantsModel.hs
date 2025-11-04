{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TenantsModel where

import Data.Text 
import Data.Aeson


-- Login Response
data TenantResponse = TenantResponse
    { 
    username :: Text
    , userid :: Text
    , statusTenant :: Text
    } deriving (Show)

instance ToJSON TenantResponse where
    toJSON (TenantResponse userName userId status) = object
        [
            "username" .= userName,
            "userid" .= userId,
            "status" .= status
        ]                           
