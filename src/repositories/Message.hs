{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Message(insertMessage, findMessage, toMessageDTO) where

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
import MessageModel

data Message f = Message
    {msgContent :: Column f Text
    , msgDate :: Column f Int64
    , msgType :: Column f Text
    , msgId :: Column f UUID
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
        , msgId = "id"
        }
    }

--Function
-- GET
findMessage :: UUID -> Connection -> IO (Either QueryError [Message Result])
findMessage id conn = do
                            let query = select $ do
                                            p <- each messageSchema
                                            where_ $ (p.msgId ==. lit id)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertMessage :: MessageRequest -> Connection -> IO (Either QueryError [UUID])
insertMessage p  conn = do
                            uuid <- nextRandom
                            run (statement () (insert1 p uuid)) conn

insert1 :: MessageRequest -> UUID -> Statement () [UUID]
insert1 p u = insert $ Insert
            { into = messageSchema
            , rows = values [ Message (lit $ p.messageContent) (lit $ p.messageDate) (lit $ p.messageType) (lit $ u) ]
            , returning = Projection (.msgId)
            , onConflict = Abort
            }


toMessageDTO :: Message Result -> MessageResponse
toMessageDTO p = MessageResponse p.msgId (Just p.msgContent) (Just p.msgDate) (Just p.msgType)



