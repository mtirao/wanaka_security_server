{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Activity(insertActivity, findActivity, findActivityAll, toActivityDTO, isSystemArmed) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import BasePrelude hiding (insert)
import Control.Monad.Trans.RWS (get)
import ActivityModel
import Data.Char (ord)
import Data.Aeson (object)

data Activity f = Activity
    {actContent :: Column f Text
    , actDate :: Column f Int64
    , actUserId :: Column f UUID
    , actId :: Column f UUID
    }
    deriving (Generic, Rel8able)
deriving stock instance f ~ Rel8.Result => Show (Activity f)

activitySchema :: TableSchema (Activity Name)
activitySchema = TableSchema
    { name = "activities"
    , schema = Nothing
    , columns = Activity
        { actContent = "activity"
        , actDate = "date"
        , actUserId= "user_id"
        , actId = "id"
        }
    }

--Function
-- GET
lastActivityQuery :: Connection -> IO (Either QueryError [Activity Result])
lastActivityQuery conn = do let query = select $ limit 1 $ orderBy (actDate >$< desc) $ each activitySchema
                            run (statement () query ) conn

isSystemArmed :: Connection -> IO Bool
isSystemArmed conn = do result <- lastActivityQuery conn
                        case result of
                            Left _ -> return False
                            Right [a] -> return $ a.actContent /= "disarm"

findActivity :: UUID -> Connection -> IO (Either QueryError [Activity Result])
findActivity id = do 
                    let query = select $ do
                                p <- each activitySchema
                                where_ $ p.actId ==. lit id
                                return p
                    run (statement () query )

findActivityAll :: Connection -> IO (Either QueryError [Activity Result])
findActivityAll conn = do
                        let query = select $ each activitySchema
                        run (statement () query ) conn

-- INSERT
insertActivity :: ActivityModel -> Connection -> IO (Either QueryError [UUID])
insertActivity p  conn = do
                            uuid <- nextRandom
                            run (statement () (insert1 p uuid)) conn

insert1 :: ActivityModel -> UUID -> Statement () [UUID]
insert1 p u = insert $ Insert
            { into = activitySchema
            , rows = values [ Activity (lit $ p.activityContent) (lit $ p.activityDate) (lit $ p.activityUserId) (lit $ u) ]
            , returning = Projection (.actId)
            , onConflict = Abort
            }


toActivityDTO :: Activity Result -> ActivityModel
toActivityDTO p = ActivityModel p.actContent p.actDate p.actUserId (Just p.actId)



