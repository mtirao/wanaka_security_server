{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Status(insertStatus, findStatus, toStatusDTO) where

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
import Control.Monad.Trans.RWS (get)
import StatusModel

data Status f = Status
    {stsStatus :: Column f Text
    , stsDate :: Column f Int64
    , stsUserId :: Column f UUID
    , stsId :: Column f UUID
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Status f)

statusSchema :: TableSchema (Status Name)
statusSchema = TableSchema
    { name = "statuses"
    , schema = Nothing
    , columns = Status 
        { stsStatus = "status"
        , stsDate = "date"
        , stsUserId = "user_id"
        , stsId = "id"
        }
    }

--Function
-- GET
findStatus :: UUID -> Connection -> IO (Either QueryError [Status Result])
findStatus id conn = do
                            let query = select $ do
                                            p <- each statusSchema
                                            where_ $ (p.stsId ==. lit id)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertStatus :: StatusModel -> Connection -> IO (Either QueryError [UUID])
insertStatus p  conn = do
                            uuid <- nextRandom
                            run (statement () (insert1 p uuid)) conn

insert1 :: StatusModel -> UUID -> Statement () [UUID]
insert1 p u = insert $ Insert
            { into = statusSchema
            , rows = values [ Status (lit $ p.statusStatus) (lit $ p.statusDate) (lit $ p.statusUserId) (lit $ u) ]
            , returning = Projection (.stsId)
            , onConflict = Abort
            }


toStatusDTO :: Status Result -> StatusModel
toStatusDTO p = StatusModel p.stsStatus p.stsDate p.stsUserId (Just p.stsId)



