{-# LANGUAGE OverloadedStrings #-}

module Database (initDB) where

import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite

initDB :: IO Connection
initDB = do
    conn <- open "wanaka.db"
    mapM_ (execute_ conn) [createActivity, createMessage, createProfile, 
                          createStatus, createRealm, createTenant, createToken]
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
        , "username TEXT NOT NULL,"
        , "password TEXT NOT NULL,"
        , "email TEXT NOT NULL,"
        , "role TEXT NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createStatus = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS system_status ("
        , "status TEXT NOT NULL,"
        , "date INTEGER NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createRealm = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS realms ("
        , "name TEXT NOT NULL,"
        , "description TEXT,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]
    
    createTenant = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS tenants ("
        , "name TEXT NOT NULL,"
        , "realm_id TEXT NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL,"
        , "FOREIGN KEY(realm_id) REFERENCES realms(id))"
        ]
    
    createToken = Query $ mconcat
        [ "CREATE TABLE IF NOT EXISTS tokens ("
        , "token TEXT NOT NULL,"
        , "user_id TEXT NOT NULL,"
        , "expiry INTEGER NOT NULL,"
        , "id TEXT PRIMARY KEY NOT NULL)"
        ]