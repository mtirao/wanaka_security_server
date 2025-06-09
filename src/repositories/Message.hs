{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Message(insertMessage) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Control.Monad.Trans.RWS (get)
import MessageModel

data Message f = Message
    {msgContent :: Column f Text
    , msgDate :: Column f Int64
    , msgType :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Message f)

messageSchema :: TableSchema (Message Name)
messageSchema = TableSchema
    { name = "messages"
    , schema = Nothing
    , columns = Message
        { msgContent = "content"
        , msgDate = "date"
        , msgType= "type"
        }
    }

--Function
-- INSERT
insertMessage :: MessageRequest -> Connection -> IO (Either QueryError [Text])
insertMessage p  conn = do
                            run (statement () (insert1 p)) conn

insert1 :: MessageRequest -> Statement () [Text]
insert1 p = insert $ Insert
            { into = messageSchema
            , rows = values [ Message (lit $ p.messageContent) (lit $ p.messageDate) (lit $ p.messageType) ]
            , returning = Projection (.msgType)
            , onConflict = Abort
            }


