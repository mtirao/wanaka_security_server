{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TenantsModel where
    
import Data.Text 
import Data.Aeson


-- Login Response
data TenantRequest = TenantRequest { user :: Text
    , password :: Text
    } deriving (Show)


instance FromJSON TenantRequest where
    parseJSON (Object v) = TenantRequest <$>
        v .: "user" <*>
        v .: "password" 

                            
