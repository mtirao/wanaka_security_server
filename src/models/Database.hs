{-# LANGUAGE OverloadedStrings #-}

module Database (initDB, closeDB) where

import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite

initDB :: IO Connection
initDB = do
    conn <- open "wanaka.db"
    mapM_ (execute_ conn) [createActivity, createMessage, createProfile, 
                          createStatus, createTenant]
    return conn
  where
    createActivity = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS activities ("
        , "activity TEXT NOT NULL,"
        , "date INTEGER NOT NULL,"
        , "user_id TEXT NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createMessage = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS messages ("
        , "content TEXT NOT NULL,"
        , "date INTEGER NOT NULL,"
        , "type TEXT NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]

    createProfile = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS profiles ("
        , "cell_phone TEXT NOT NULL,"
        , "email TEXT NOT NULL,"
        , "first_name TEXT NOT NULL,"
        , "last_name TEXT NOT NULL,"
        , "phone TEXT NOT NULL,"
        , "gender TEXT NOT NULL,"
        , "address TEXT NOT NULL,"
        , "city TEXT NOT NULL,"
        , "user_id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createStatus = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS system_status ("
        , "status TEXT NOT NULL,"
        , "date INTEGER NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createTenant = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS tenants ("
        , "username TEXT PRIMARY KEY NOT NULL,"
        , "password TEXT NOT NULL,"
        , "user_id TEXT NOT NULL,"
        , "created_at INTEGER NOT NULL,"
        , "status TEXT NOT NULL)"
        ]

-- Close the database connection
closeDB :: Connection -> IO ()
closeDB conn = SQLite.close conn    