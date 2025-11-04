{-# LANGUAGE OverloadedStrings #-}

module TenantController where

import qualified Web.Scotty as WS
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.Time
import Data.Time.Clock.POSIX
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.UUID.V4 (nextRandom)
import GHC.Int
import TenantsModel
import Tenant

createTenant body conn =  do
        b <- body
        uuid <- liftIO nextRandom
        curTime <- liftIO getPOSIXTime
        let curTimeInt = floor . nominalDiffTimeToSeconds $ curTime
        let tenant = (decode b :: Maybe TenantRequest) 
        case tenant of
            Nothing -> WS.status badRequest400
            Just a -> do                   
                result <- liftIO $ insertTenant conn (user a) (password a) uuid  curTimeInt "enabled" 
                case result of
                    Left err -> do
                        WS.status internalServerError500
                    Right _ ->
                        WS.status noContent204