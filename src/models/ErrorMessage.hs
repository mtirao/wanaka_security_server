module ErrorMessage where

import Data.Text 
import Data.Aeson

--ErrorMessage
newtype ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]

--SuccessMessage
newtype SuccessMessage = SuccessMessage Text
    deriving (Show)
    
instance ToJSON SuccessMessage where
    toJSON (SuccessMessage message) = object
        [
            "success" .= message
        ]