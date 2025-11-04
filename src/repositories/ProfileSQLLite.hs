{-# LANGUAGE OverloadedStrings #-}

module ProfileSQLLite where

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


findProfile :: Connection -> Text -> IO [ProfileModel]
findProfile conn id = 
    query conn "SELECT * FROM profiles WHERE userId = ?" (Only id)  

insertProfile :: Connection -> ProfileModel -> Text -> IO ()
insertProfile conn profile userId = do
    execute conn "INSERT INTO profiles (cell_phone, email, first_name, last_name, phone, gender, address, city, user_id) VALUES (?,?,?,?,?,?,?,?,?)"
        ( cellPhone profile
        , email profile
        , firstName profile
        , lastName profile
        , phone profile
        , gender profile
        , address profile
        , city profile
        , userId
        ) 
    return ()