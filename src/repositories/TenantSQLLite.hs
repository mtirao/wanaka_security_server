{-# LANGUAGE OverloadedStrings #-}

module TenantSQLLite where

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

data Tenant = Tenant { userName :: Text
    , userPassword :: Text
    , userId :: UUID
    , createdAt :: Int64
    , status :: Text
    } deriving (Show)

instance FromRow Tenant where
    fromRow = Tenant <$> field <*> field <*> field <*> field <*> field  

instance ToRow Tenant where
    toRow t = toRow (userName t, userPassword t, userId t, createdAt t, status t)

    
findTenantByUsername :: Connection -> Text -> IO [Tenant]
findTenantByUsername conn username = 
    query conn "SELECT * FROM tenants WHERE username = ?" (Only username)   

insertTenant :: Connection -> Text -> Text -> UUID -> Int64 -> Text -> IO ()
insertTenant conn username password userid createdAt status = do
    execute conn "INSERT INTO tenants (username, password, user_id, created_at, status) VALUES (?,?,?,?,?)"
        ( username
        , password
        , userid
        , createdAt
        , status
        ) 
    return ()

deleteTenant :: Connection -> UUID -> IO ()
deleteTenant conn userid = do
    execute conn "DELETE FROM tenants WHERE user_id = ?" (Only userid)
    return ()

updatePassword :: Connection -> UUID -> Text -> IO ()
updatePassword conn userid newPassword = do
    execute conn "UPDATE tenants SET password = ? WHERE user_id = ?" (newPassword, userid)
    return ()

