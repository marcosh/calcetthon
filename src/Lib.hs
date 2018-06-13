{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    , api
    ) where

import           Api                              hiding (pool)
import           Control.Monad.Logger             (runStderrLoggingT)
import           Data.Pool                        (Pool)
import           Database.Persist.Postgresql      (createPostgresqlPool)
import           Database.Persist.Sql             (SqlBackend, runMigration,
                                                   runSqlPool)
import           Eventful.Store.Sql.DefaultEntity (migrateSqlEvent)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

pool :: IO (Pool SqlBackend)
pool = runStderrLoggingT $ createPostgresqlPool conn 10 where
    conn = "host=localhost dbname=calcetthon user=calcetthon password=calcetthon port=5432"

startApp :: IO ()
startApp = do
    runSqlPool (runMigration migrateSqlEvent) =<< pool
    run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = calcetthonApi
