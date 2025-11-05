{-# LANGUAGE OverloadedStrings #-}

module Profile where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import Data.UUID (UUID, fromText, toText, toString, fromString)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import GHC.Int (Int64)
import ProfileModel (ProfileModel(..))
import SQLModel
import Control.Exception (try)


findProfile :: Connection -> UUID -> IO (Either DBError ProfileModel)
findProfile conn id = do
    result <- try $ query conn "SELECT cell_phone, email, first_name, last_name, phone, gender, address, city, user_id FROM profiles WHERE user_id = ?" (Only id)  
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right [] -> return $ Left $ ProfileNotFound id
        Right [profile] -> return $ Right profile
        Right _ -> return $ Left $ DatabaseError (SQLError ErrorError "2" "Multiple profiles found for single ID")



insertProfile :: Connection -> ProfileModel -> UUID -> IO (Either DBError ())
insertProfile conn profile userId = do
    let query = "INSERT INTO profiles (cell_phone, email, first_name, last_name, phone, gender, address, city, user_id) VALUES (?,?,?,?,?,?,?,?,?)"
    res <- try $ execute conn query
        ( cellPhone profile
        , email profile
        , firstName profile
        , lastName profile
        , phone profile
        , gender profile
        , address profile
        , city profile
        , userId
        ) :: IO (Either SQLError ())
    case res of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting profile")
        Right _ ->  return $ Right ()