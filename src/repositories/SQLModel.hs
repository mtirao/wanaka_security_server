{-# LANGUAGE OverloadedStrings #-}

module SQLModel where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import Data.UUID (UUID, fromText, toText, toString, fromString)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text, pack)


-- Add FromField instance for UUID
instance FromField UUID where
    fromField f = do
        txt <- fromField f
        case fromText txt of
            Just uuid -> return uuid
            Nothing -> fail "Invalid UUID in database"

-- Add ToField instance for UUID
instance ToField UUID where
    toField = toField . toText

uuidToText :: UUID -> Text
uuidToText = pack . toString