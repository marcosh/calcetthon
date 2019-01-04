{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    , api
    ) where

import           Api                                  hiding (pool)
import           Migrations                           (migrateReadModel)

-- eventful-sql-common
import           Eventful.Store.Sql.DefaultEntity     (migrateSqlEvent)

-- monad-logger
import           Control.Monad.Logger                 (runStderrLoggingT)

-- resource-pool
import           Data.Pool                            (Pool)

-- persistent
import           Database.Persist.Sql                 (SqlBackend, runMigration,
                                                       runSqlPool)

-- persistent-postgres
import           Database.Persist.Postgresql          (createPostgresqlPool)

-- servant-server
import           Servant

-- wai-cors
import           Network.Wai.Middleware.Cors          (simpleCors)

-- wai-extra
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- warp
import           Network.Wai.Handler.Warp

pool :: IO (Pool SqlBackend)
pool = runStderrLoggingT $ createPostgresqlPool conn 10 where
    conn = "host=localhost dbname=calcetthon user=calcetthon password=calcetthon port=5432"

startApp :: IO ()
startApp = do
    let migrations = do
            migrateSqlEvent
            migrateReadModel
    runSqlPool (runMigration migrations) =<< pool
    run 8080 app

app :: Application
app = logStdoutDev $ simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = calcetthonApi
