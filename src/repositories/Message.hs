{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Message(insertMessage, findMessage, findMessageAll, toMessageDTO, isSystemArmed) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Maybe
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Control.Monad.Trans.RWS (get)
import MessageModel
import ActivityModel
import Activity

data Message f = Message
    {msgContent :: Column f Text
    , msgDate :: Column f Int64
    , msgType :: Column f MessageType
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
                                            where_ (p.msgId ==. lit id)
                                            return p
                            run (statement () query ) conn

findMessageAll :: Connection -> IO (Either QueryError [Message Result])
findMessageAll conn = do
                            let query = select $ each messageSchema
                            run (statement () query ) conn

-- INSERT
insertMessage :: MessageModel -> Connection -> IO (Either QueryError [UUID])
insertMessage p  conn = do
                            uuid <- nextRandom
                            run (statement () (insert1 p uuid)) conn

insert1 :: MessageModel -> UUID -> Statement () [UUID]
insert1 p u = insert $ Insert
            { into = messageSchema
            , rows = values [ Message (lit p.messageContent) (lit p.messageDate) (lit p.messageType) (lit u) ]
            , returning = Projection (.msgId)
            , onConflict = Abort
            }

triggerAlert :: MessageType -> IO ()
triggerAlert m = if m == Alert
then do
    -- Logic to trigger alert
    putStrLn "Alert triggered"
else do
    putStrLn "No alert"

isSystemArmed :: Connection -> IO Bool
isSystemArmed conn = do
                        result <- lastActivityQuery conn
                        case result of
                            Left _ -> return False
                            Right [a] -> case getContentType a of
                                            Armed -> return True
                                            ArmedStay -> return True
                                            ArmedAway -> return True
                                            ArmeCustom -> return True
                                            Disarmed -> return False



toMessageDTO :: Message Result -> MessageModel
toMessageDTO p = MessageModel p.msgContent p.msgDate p.msgType (Just p.msgId)



