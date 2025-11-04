{-# LANGUAGE OverloadedStrings #-}

module TokenSQLLite where

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

data Token = Token
    {authtoken :: Text
    , clientid :: Text
    }
    deriving (Show)

instance FromRow Token where
    fromRow = Token <$> field <*> field 
instance ToRow Token where
    toRow t = toRow (authtoken t, clientid t)   


findToken :: Connection -> Text -> IO [Token]
findToken conn token = 
    query conn "SELECT * FROM tokens WHERE authtoken = ?" (Only token)

insertToken :: Connection -> Text -> Text -> IO ()
insertToken conn a c = do
    execute conn "INSERT INTO tokens (authtoken, clientid) VALUES (?,?)"
        (a, c)

deleteToken :: Connection -> Text -> IO ()
deleteToken conn token = do
    execute conn "DELETE FROM tokens WHERE authtoken = ?" (Only token)

updateToken :: Connection -> Text -> Text -> IO ()
updateToken conn token newClientId = do
    execute conn "UPDATE tokens SET clientid = ? WHERE authtoken = ?" (newClientId, token)

getClientId :: Connection -> Text -> IO (Maybe Text)
getClientId conn token = do
    results <- query conn "SELECT clientid FROM tokens WHERE authtoken = ?" (Only token) :: IO [Only Text]
    case results of
        [Only cid] -> return (Just cid)
        _          -> return Nothing        