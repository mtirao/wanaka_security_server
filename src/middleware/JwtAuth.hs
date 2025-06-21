{-# LANGUAGE OverloadedStrings #-}

module JwtAuth(createJwt, jwtAuthMiddleware) where

import Web.Scotty ( body, header, status, next,  ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class

import GHC.Int

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))
import Network.Wai.Middleware.HttpAuth
import ErrorMessage
import Views
import qualified Token as Tkn
import Realm
import TokenModel
import Control.Monad.Trans.Class (MonadTrans(lift))

import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)


--- Jwt create token
createJwt conn clientid granttype= do
        curTime <- liftIO getPOSIXTime
        let expDate = tokenExpiration curTime
        let token = buildToken granttype expDate
        liftIO $ print ("Creating token for client: " <>  show clientid <> " with expiration: " <> show expDate)
        liftIO $ Tkn.insertToken token clientid conn
        jsonResponse (TokenResponse (buildToken granttype expDate) "JWT" "" )


-- Jwt Middleware
jwtAuthMiddleware :: Connection -> Middleware
jwtAuthMiddleware conn app req respond = 
                let url = rawPathInfo req <> rawQueryString req
                in if url == "/api/wanaka/accounts/login"
                    then app req respond
                    else case lookup "authorization" (requestHeaders req) of
                        Just bs -> do
                            let tokenBs = case TL.stripPrefix "Bearer "  (TL.fromStrict (T.decodeUtf8 bs)) of
                                            Just t  -> t
                                            Nothing -> (TL.fromStrict (T.decodeUtf8 bs))
                            let tokenText = TL.toStrict tokenBs
                            result <- liftIO $ Tkn.findToken tokenText conn
                            liftIO $ print ("Checking token: " <> show (T.decodeUtf8 bs))
                            case result of
                                Left _ -> 
                                    respond $ responseLBS status401 [] "Unauthorized"
                                Right token -> case token of
                                    [] -> 
                                        respond $ responseLBS status401 [] "Unauthorized"
                                    [t] -> 
                                        app req respond
                        _ -> 
                            respond $ responseLBS status401 [] "Unauthorized"
                        

-- Token helpers
convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)

buildToken :: Text -> Int64 -> Text
buildToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> T.decodeUtf8 jwt
                    where
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload

decodeToken :: Text -> Maybe Payload
decodeToken t = case token of
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" $ tokenFromHeader (TL.fromStrict t)
                    tokenFromHeader t = BL.toStrict $ TL.encodeUtf8 t


--- Helper Functions
convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = toInt64 u + 864000

toInt64 :: NominalDiffTime -> Int64
toInt64 = floor . nominalDiffTimeToSeconds

tokenExperitionTime :: Payload -> Int64
tokenExperitionTime (Payload u e) = e