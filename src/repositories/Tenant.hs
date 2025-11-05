{-# LANGUAGE OverloadedStrings #-}

module Tenant where

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
import SQLModel
import Control.Exception (try)

data Tenant = Tenant { userName :: Text
    , userPassword :: Text
    , userId :: UUID
    , createdAt :: Int64
    , statusTenant :: Text
    } deriving (Show)

instance FromRow Tenant where
    fromRow = Tenant <$> field <*> field <*> field <*> field <*> field  

instance ToRow Tenant where
    toRow t = toRow (userName t, userPassword t, userId t, createdAt t, statusTenant t)

    
findTenantByUsername :: Connection -> Text -> IO (Either DBError Tenant)
findTenantByUsername conn username = do
    result <- try $ query conn "SELECT * FROM tenants WHERE username = ?" (Only username)  
    case result of
        Left err -> return $ Left $ DatabaseError err
        Right [] -> return $ Left $ TenantNotFound username
        Right [tenant] -> return $ Right tenant
        Right _ -> return $ Left $ DatabaseError (SQLError ErrorError "2" "Multiple profiles found for single ID")


insertTenant :: Connection -> Text -> Text -> UUID -> Int64 -> Text -> IO (Either DBError UUID)
insertTenant conn username password userid createdAt status = do
    let query = "INSERT INTO tenants (username, password, user_id, created_at, status) VALUES (?,?,?,?,?)"
    result <- try $ execute conn query
        ( username
        , password
        , userid
        , createdAt
        , status
        ) :: IO (Either SQLError ())
    case result of
        Left err -> return $ Left $ DatabaseError (SQLError ErrorError "1" "Error inserting tenant")
        Right _ ->  return $ Right userid

deleteTenant :: Connection -> UUID -> IO ()
deleteTenant conn userid = do
    execute conn "DELETE FROM tenants WHERE user_id = ?" (Only userid)
    return ()

updatePassword :: Connection -> UUID -> Text -> IO ()
updatePassword conn userid newPassword = do
    execute conn "UPDATE tenants SET password = ? WHERE user_id = ?" (newPassword, userid)
    return ()

